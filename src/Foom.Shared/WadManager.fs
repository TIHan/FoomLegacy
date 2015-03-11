namespace Foom.Shared.WadManager

open System
open System.IO
open System.Numerics
open System.Drawing

open Foom.Shared.Wad.Pickler

open Foom.Shared.Level
open Foom.Shared.Level.Structures

open FSharp.LitePickler.Core
open FSharp.LitePickler.Unpickle

type WadManager = 
    {
        fileName: string
        wadData: WadData
        defaultPaletteData: PaletteData option
        flats: Bitmap []
    }

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module WadManager =

    open Foom.Shared.Wad.Pickler.UnpickleWad       

    let inline (>=>) (f1: 'a -> Async<'b>) (f2: 'b -> Async<'c>) : 'a -> Async<'c> =
        fun a -> async {
            let! a = f1 a
            return! f2 a
        }

    let runUnpickle u fileName = async { 
        use file = File.Open(fileName, FileMode.Open)
        return u_run u (LiteReadStream.ofStream file) 
    }

    let runUnpickles us fileName = async {
        use file = File.Open(fileName, FileMode.Open)
        let stream = LiteReadStream.ofStream file
        return us |> Array.map (fun u -> u_run u stream)
    }

    let loadLump u (header: LumpHeader) fileName = 
        runUnpickle (u header.Size (int64 header.Offset)) fileName

    let loadLumpMarker u (markerStart: LumpHeader) (markerEnd: LumpHeader) fileName =
        runUnpickle (u (markerEnd.Offset - markerStart.Offset) (int64 markerStart.Offset)) fileName

    let loadLumps u (headers: LumpHeader []) fileName =
        let us =
            headers
            |> Array.map (fun h -> u h.Size (int64 h.Offset))
        runUnpickles us fileName

    let loadPalettes wad =
        match wad.wadData.LumpHeaders |> Array.tryFind (fun x -> x.Name.ToUpper () = "PLAYPAL") with
        | None -> async { return wad }
        | Some lumpPaletteHeader -> async {
            let! lumpPalettes = loadLump u_lumpPalettes lumpPaletteHeader wad.fileName
            return { wad with defaultPaletteData = Some lumpPalettes.[0] }
        }
        
    let loadFlats wm =
        match wm.defaultPaletteData with
        | None ->
            printfn "Warning: Unable to load flat textures because there is no default palette."
            async { return wm }
        | Some palette ->
            let fileName = wm.fileName
            let lumpHeaders = wm.wadData.LumpHeaders

            let lumpFlatsHeaderStartIndex = lumpHeaders |> Array.tryFindIndex (fun x -> x.Name.ToUpper () = "F_START")
            let lumpFlatsHeaderEndIndex = lumpHeaders |> Array.tryFindIndex (fun x -> x.Name.ToUpper () = "F_END")

            match lumpFlatsHeaderStartIndex, lumpFlatsHeaderEndIndex with
            | None, None -> 
                async { return wm }

            | Some _, None ->
                printfn """Warning: Unable to load flat textures because "F_END" lump was not found."""
                async { return wm }

            | None, Some _ ->
                printfn """Warning: Unable to load flat textures because "F_START" lump was not found."""
                async { return wm }

            | Some lumpFlatsHeaderStartIndex, Some lumpFlatsHeaderEndIndex ->
                let lumpFlatHeaders =
                    lumpHeaders.[(lumpFlatsHeaderStartIndex + 1)..(lumpFlatsHeaderEndIndex - 1)]

                // Assert Flat Headers are valid
                lumpFlatHeaders
                |> Array.iter (fun h ->
                    if h.Offset.Equals 0 then failwithf "Invalid flat header, %A. Offset is 0." h
                    if not (h.Size.Equals 4096) then failwithf "Invalid flat header, %A. Size is not 4096." h)

                async {
                    let! lumpFlats = loadLumps u_lumpRaw lumpFlatHeaders fileName

                    let flats =
                        lumpFlats
                        |> Array.map (fun x ->
                            x |> Array.map (fun y -> palette.Pixels.[int y]))
                        |> Array.map (fun pixels ->
                            let bmp = new Bitmap (64, 64)

                            for i = 0 to 64 - 1 do
                                for j = 0 to 64 - 1 do
                                    let pixel = pixels.[i + (j * 64)]
                                    bmp.SetPixel (i, j, Drawing.Color.FromArgb (int pixel.R, int pixel.G, int pixel.B))
                            bmp)

                    return { wm with flats = flats }
                }

    let create initialWadFileName = async {
        let! wadData = runUnpickle u_wad initialWadFileName

        return!
            { fileName = initialWadFileName; wadData = wadData; defaultPaletteData = None; flats = [||] }
            |> (loadPalettes >=> loadFlats)
    }

    let findLevel (levelName: string) wm =
        let fileName = wm.fileName
        let name = levelName.ToLower ()

        match
            wm.wadData.LumpHeaders
            |> Array.tryFindIndex (fun x -> x.Name.ToLower () = name.ToLower ()) with
        | None -> async { return failwithf "Unable to find level, %s." name }
        | Some lumpLevelStartIndex ->

        printfn "Found Level: %s" name
        let lumpHeaders = wm.wadData.LumpHeaders.[lumpLevelStartIndex..]

        let lumpLinedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "LINEDEFS".ToLower ())
        let lumpSidedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SIDEDEFS".ToLower ())
        let lumpVerticesHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "VERTEXES".ToLower ())
        let lumpSectorsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SECTORS".ToLower ())

        async {
            let! lumpVertices = loadLump u_lumpVertices lumpVerticesHeader fileName
            let! lumpSidedefs = loadLump u_lumpSidedefs lumpSidedefsHeader fileName
            let! lumpLinedefs = loadLump (u_lumpLinedefs lumpVertices.Vertices lumpSidedefs.Sidedefs) lumpLinedefsHeader fileName
            let! lumpSectors = loadLump (u_lumpSectors lumpLinedefs.Linedefs) lumpSectorsHeader fileName

            let sectors : Sector [] =
                lumpSectors.Sectors
                |> Array.mapi (fun i sector ->
                    let lines =
                        sector.Linedefs
                        |> Array.map (
                            function 
                            | LinedefData.Doom (x, y, f, b, data) -> 
                                { Start = x
                                  End = y
                                  FrontSidedef = match f with | Some f when f.SectorNumber = i -> Some (Sidedef ()) | _ -> None
                                  BackSidedef = match b with | Some b when b.SectorNumber = i -> Some (Sidedef ()) | _ -> None }
                                |> Some)
                        |> Array.filter (fun x -> x.IsSome)
                        |> Array.map (fun x -> x.Value)
                    { Linedefs = lines }
                )

            return { Sectors = sectors }
        }