namespace Foom.Shared.Wad

open System

open Foom.Shared.Level

[<Sealed>]
type Wad =
    interface IDisposable

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Wad =

    val openFile : fileName: string -> Wad

    val loadLevel : name: string -> wad: Wad -> Level
