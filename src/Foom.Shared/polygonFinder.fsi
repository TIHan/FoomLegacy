// Ideas from Pascal vd Heiden's Doom Builder 2 C# triangulation code.
[<RequireQualifiedAccess>]
module Foom.Shared.PolygonFinder

// Finds polygons from a sector.
val findPolygons : Sector -> Polygon list
