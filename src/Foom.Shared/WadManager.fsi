namespace Foom.Shared.WadManager

open System

open Foom.Shared.Level

[<Sealed>]
type WadManager

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module WadManager =

    val create : initialWadFileName: string -> Async<WadManager>

    val findLevel : levelName: string -> wm: WadManager -> Async<Level>
