namespace Foom.Shared.IO

open System.IO

open Foom.Shared.Wad
open Foom.Shared.Level

type WadFile = private {
    File: FileStream
    Wad: Wad }

module WadManager =
    val openWad : fileName: string -> WadFile

    val closeWad : wad: WadFile -> unit

    val loadLevel : name: string -> wad: WadFile -> Level
