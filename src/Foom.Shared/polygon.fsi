namespace Foom.Shared

open System.Numerics

[<Struct>]
type Edge = 
    val X : Vector2
    val Y : Vector2

type Polygon

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Polygon =
    val create : vertices: Vector2 [] -> Polygon

    val addChild : child: Polygon -> poly: Polygon -> Polygon

    val addChildren : children: Polygon list -> poly: Polygon -> Polygon

    val vertices : poly: Polygon -> Vector2 []

    val children : poly: Polygon -> Polygon list

    val edges : poly: Polygon -> Edge list

    val isArrangedClockwise : poly: Polygon -> bool

    val isPointInside : point: Vector2 -> poly: Polygon -> bool