namespace Foom.Shared.Level.Structures

open Foom.Shared.Numerics
open System.Numerics

[<NoComparison; ReferenceEquality>]
type Linedef = {
    Start: Vector2
    End: Vector2
    FrontSidedef: Sidedef option
    BackSidedef: Sidedef option } with

    member this.Angle =
        let v = this.End - this.Start
        Vec2.angle v  
