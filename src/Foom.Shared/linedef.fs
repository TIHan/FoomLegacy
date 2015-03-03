namespace Foom.Shared.Level.Structures

open Foom.Shared.Numerics
open System.Numerics

[<NoComparison; ReferenceEquality>]
type Linedef = {
    Start: Vector2
    End: Vector2
    FrontSidedef: Sidedef option
    BackSidedef: Sidedef option }

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Linedef =

    // http://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line
    let inline private isPointOnLeftSide (v1: Vector2) (v2: Vector2) (p: Vector2) =
        (v2.X - v1.X) * (p.Y - v1.Y) - (v2.Y - v1.Y) * (p.X - v1.X) > 0.f

    let angle (linedef: Linedef) =
        let v = linedef.End - linedef.Start
        Vec2.angle v  

    let isPointInFrontOfFacingSide (p: Vector2) (linedef: Linedef) =
        match linedef.FrontSidedef, linedef.BackSidedef with
        | None, None -> false
        | Some _, Some _ -> true
        | _ ->
            if linedef.FrontSidedef.IsSome
            then isPointOnLeftSide linedef.End linedef.Start p
            else isPointOnLeftSide linedef.Start linedef.End p
            

