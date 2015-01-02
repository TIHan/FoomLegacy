[<RequireQualifiedAccess>]
module Foom.Shared.Triangulation.EarClipping

open System.Numerics

open Foom.Shared

let compute (poly: Polygon) = List.empty<Vector2 []>