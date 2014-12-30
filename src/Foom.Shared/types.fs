namespace Foom.Shared

open System
open System.Numerics

module Vec2 =
    let angle (v: Vector2) =
        -(single <| atan2 (float -v.Y) (float v.X)) + single Math.PI * 0.5f

type Sidedef = Sidedef of unit

[<NoComparison; ReferenceEquality>]
type Linedef = {
    Start: Vector2
    End: Vector2
    FrontSidedef: Sidedef option
    BackSidedef: Sidedef option } with

    member this.Angle =
        let v = this.End - this.Start
        Vec2.angle v         

type Sector = {
    Linedefs: Linedef [] }

type Level = {
    Sectors: Sector [] }

