namespace Foom.Shared

open System.Numerics

[<Struct>]
type Triangle2 = 
    val X : Vector2
    val Y : Vector2
    val Z : Vector2

    new (x, y, z) = { X = x; Y = y; Z = z }

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle2 =
    let create x y z = Triangle2 (x, y, z)

    let toPolygon (tri: Triangle2) = Polygon.create [|tri.X;tri.Y;tri.Z|]

    // http://www.blackpawn.com/texts/pointinpoly/
    let isPointInside point (tri: Triangle2) =
        // Compute vectors        
        let v0 = tri.Z - tri.X
        let v1 = tri.Y - tri.X
        let v2 = point - tri.X

        // Compute dot products
        let dot00 = Vector2.Dot (v0, v0)
        let dot01 = Vector2.Dot (v0, v1)
        let dot02 = Vector2.Dot (v0, v2)
        let dot11 = Vector2.Dot (v1, v1)
        let dot12 = Vector2.Dot (v1, v2)

        // Compute barycentric coordinates
        let invDenom = 1.f / (dot00 * dot11 - dot01 * dot01)
        let u = (dot11 * dot02 - dot01 * dot12) * invDenom
        let v = (dot00 * dot12 - dot01 * dot02) * invDenom

        // Check if point is in triangle
        (u >= 0.f) && (v >= 0.f) && (u + v < 1.f)