namespace Foom.Shared.IO

open System
open System.IO
open System.Numerics

open FSharp.LitePickler.Unpickle

open Foom.Shared.Wad

open Foom.Shared.Level
open Foom.Shared.Level.Structures

type WadFile = private {
    File: FileStream
    Wad: Wad }

module WadManager =
    let openWad fileName =
        let file = File.Open (fileName, FileMode.Open)
        let wad = u_run UnpickleWad.u_wad <| LiteReadStream.ofStream file
        file.Position <- 0L
        { File = file; Wad = wad }

    let closeWad (wad: WadFile) =
        wad.File.Close ()
        wad.File.Dispose ()

    let loadLevel (name: string) (wad: WadFile) : Level =
        let name = name.ToLower ()

        let lumpLevelStartIndex =
            wad.Wad.LumpHeaders
            |> Array.findIndex (fun x -> x.Name.ToLower () = name)

        printfn "Found Level: %s" name
        let lumpHeaders = wad.Wad.LumpHeaders.[lumpLevelStartIndex..]

        let lumpLinedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "LINEDEFS".ToLower ())
        let lumpSidedefsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SIDEDEFS".ToLower ())
        let lumpVerticesHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "VERTEXES".ToLower ())
        let lumpSectorsHeader = lumpHeaders |> Array.find (fun x -> x.Name.ToLower () = "SECTORS".ToLower ())

        let loadLump u (header: LumpHeader) =
            let l = u_run (u header.Size (int64 header.Offset)) <| LiteReadStream.ofStream wad.File
            wad.File.Position <- 0L
            l

        let lumpVertices = loadLump UnpickleWad.u_lumpVertices lumpVerticesHeader
        let lumpSidedefs = loadLump UnpickleWad.u_lumpSidedefs lumpSidedefsHeader
        let lumpLinedefs = loadLump (UnpickleWad.u_lumpLinedefs lumpVertices.Vertices lumpSidedefs.Sidedefs) lumpLinedefsHeader
        let lumpSectors = loadLump (UnpickleWad.u_lumpSectors lumpLinedefs.Linedefs) lumpSectorsHeader

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
