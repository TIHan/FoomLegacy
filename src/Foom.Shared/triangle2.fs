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