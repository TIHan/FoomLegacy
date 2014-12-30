namespace Foom.Shared

open System.Numerics

type Polygon = { Vertices: Vector2 []; Children: Polygon list }

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Polygon =
    // http://alienryderflex.com/polygon/
    let isPointInside (point: Vector2) (poly: Polygon) =
        let vertices =  poly.Vertices
        let mutable j = vertices.Length - 1
        let mutable c = false

        for i = 0 to vertices.Length - 1 do
            let xp1 = vertices.[i].X
            let xp2 = vertices.[j].X
            let yp1 = vertices.[i].Y
            let yp2 = vertices.[j].Y

            if
                ((yp1 > point.Y) <> (yp2 > point.Y)) &&
                (point.X < (xp2 - xp1) * (point.Y - yp1) / (yp2 - yp1) + xp1) then
                c <- not c
            else ()

            j <- i
        c