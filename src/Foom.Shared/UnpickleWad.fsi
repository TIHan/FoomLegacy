module Foom.Shared.Wad.UnpickleWad

open System
open System.Numerics
open FSharp.LitePickler.Core
open FSharp.LitePickler.Unpickle

val u_lumpHeader : Unpickle<LumpHeader>

val u_lumpHeaders : count: int -> offset: int64 -> Unpickle<LumpHeader []>

val u_wad : Unpickle<Wad>

val u_lumpThings : format: ThingDataFormat -> size: int -> offset: int64 -> Unpickle<LumpThings>

val u_lumpVertices : size: int -> offset: int64 -> Unpickle<LumpVertices>

val u_lumpSidedefs : size: int -> offset: int64 -> Unpickle<LumpSidedefs>

val u_lumpLinedefs : vertices: Vector2 [] -> sidedefs: SidedefData [] -> size: int -> offset: int64 -> Unpickle<LumpLinedefs>

val u_lumpSectors : linedefs: LinedefData [] -> size: int -> offset: int64 -> Unpickle<LumpSectors>
