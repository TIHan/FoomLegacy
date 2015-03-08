namespace Foom.Shared.Wad

open System
open System.IO
open System.Numerics

open Foom.Shared.Wad.Pickler

open Foom.Shared.Level
open Foom.Shared.Level.Structures

type Wad = {
    file: FileStream
    data: WadData } with

    interface IDisposable with
        member this.Dispose () =
            this.file.Close ()
            this.file.Dispose ()

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Wad =
    
    open FSharp.LitePickler.Core
    open FSharp.LitePickler.Unpickle

    open Foom.Shared.Wad.Pickler.UnpickleWad

    let openFile fileName =
        let file = File.Open (fileName, FileMode.Open)
        let wad = u_run u_wad <| LiteReadStream.ofStream file
        file.Position <- 0L
        { file = file; data = wad }

    let loadLevel (name: string) (wad: Wad) : Level =
        let name = name.ToLower ()

        let lumpLevelStartIndex =
            wad.data.LumpHeaders
            |> Array.findIndex (fun x -> x.Name.ToLower () = name)

        printfn "Found Level: %s" name
        let lumpHeaders = wad.data.LumpHeaders.[lumpLevelStartIndex..]

        let lumpLinedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "LINEDEFS".ToLower ())
        let lumpSidedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SIDEDEFS".ToLower ())
        let lumpVerticesHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "VERTEXES".ToLower ())
        let lumpSectorsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SECTORS".ToLower ())
        let lumpFlatsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToUpper () = "F_START")
        let lumpFlatsHeaderEnd = lumpHeaders |> Array.find (fun x -> x.Name.ToUpper () = "F_END")
        let lumpPaletteHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToUpper () = "PLAYPAL")
        let lumpColormapHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToUpper () = "COLORMAP")

        let loadLump u (header: LumpHeader) =
            let l = u_run (u header.Size (int64 header.Offset)) <| LiteReadStream.ofStream wad.file
            wad.file.Position <- 0L
            l

        let loadLumpMarker u (markerStart: LumpHeader) (markerEnd: LumpHeader) =
            let l = u_run (u (markerEnd.Offset - markerStart.Offset) (int64 markerStart.Offset)) <| LiteReadStream.ofStream wad.file
            wad.file.Position <- 0L
            l

        let lumpVertices = loadLump u_lumpVertices lumpVerticesHeader
        let lumpSidedefs = loadLump u_lumpSidedefs lumpSidedefsHeader
        let lumpLinedefs = loadLump (u_lumpLinedefs lumpVertices.Vertices lumpSidedefs.Sidedefs) lumpLinedefsHeader
        let lumpSectors = loadLump (u_lumpSectors lumpLinedefs.Linedefs) lumpSectorsHeader
        let lumpFlats = loadLumpMarker u_lumpFlats lumpFlatsHeader lumpFlatsHeaderEnd

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

        { Sectors = sectors }
