// Based off of Pascal vd Heiden's Doom Builder 2 C# triangulation code.
[<RequireQualifiedAccess>]
module Foom.Shared.PolygonFinder

// Find polygons from a sector.
val find : Sector -> Polygon list
