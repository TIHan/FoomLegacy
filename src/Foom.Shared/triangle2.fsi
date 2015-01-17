namespace Foom.Shared

open System.Numerics

[<Struct>]
type Triangle2 = 
    val X : Vector2
    val Y : Vector2
    val Z : Vector2

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Triangle2 =
    val create : Vector2 -> Vector2 -> Vector2 -> Triangle2

    val toPolygon : Triangle2 -> Polygon
