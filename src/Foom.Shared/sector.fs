namespace Foom.Shared.Level.Structures

open System
open System.Numerics
open System.Collections.Immutable

open Foom.Shared.Geometry
open Foom.Shared.Level.Structures

type Sector = {
    Linedefs: Linedef [] }

[<AutoOpen>]
module Tracer =
    type Tracer =
        private { 
            endVertex: Vector2
            currentVertex: Vector2
            linedefs: Linedef list
            visitedLinedefs: ImmutableHashSet<Linedef>
            path: Linedef list }

        static member private FindClosestLinedef (linedefs: Linedef list) =
            let s = linedefs |> List.minBy (fun x -> x.Start.X)
            let e = linedefs |> List.minBy (fun x -> x.End.X)

            let v =
                if s.Start.X <= e.End.X 
                then s.Start
                else e.End

            match linedefs |> List.tryFind (fun x -> x.Start.Equals v) with
            | None -> linedefs |> List.find (fun x -> x.End.Equals v)
            | Some linedef -> linedef

        member inline private this.NonVisitedLinedefs () = 
            this.linedefs |> List.filter (not << this.visitedLinedefs.Contains)

        member private this.Visit (linedef: Linedef) =
            { this with
                currentVertex = if linedef.FrontSidedef.IsSome then linedef.End else linedef.Start
                visitedLinedefs = this.visitedLinedefs.Add linedef
                path = linedef :: this.path }  

        member private this.IsFinished = this.currentVertex.Equals this.endVertex   

        member private this.TryVisitNextLinedef () =
            match this.IsFinished with
            | true -> this, false
            | _ ->
                let currentLinedef = this.path.Head
                let currentVertex = this.currentVertex
                let visitedLinedefs = this.visitedLinedefs

                match
                    this.linedefs
                    |> List.filter (fun l -> 
                        (currentVertex.Equals l.Start || currentVertex.Equals l.End) && not (visitedLinedefs.Contains l)) with
                | [] -> this, false
                | [linedef] -> this.Visit linedef, true
                | linedefs ->
                    let p1 =
                        if currentVertex.Equals currentLinedef.Start
                        then currentLinedef.End
                        else currentLinedef.Start

                    let dir1 = Vector2.Normalize (p1 - currentVertex)

                    let linedef =
                        linedefs
                        |> List.minBy (fun x ->
                            let p2 =
                                if currentVertex.Equals x.Start
                                then x.End
                                else x.Start
                            
                            let result = Vector2.Dot (dir1, Vector2.Normalize (currentVertex - p2))
                            if Linedef.isPointOnFrontSide p2 currentLinedef
                            then result
                            else 2.f + (result * -1.f))

                    this.Visit linedef, true

        member this.Run () =
            let linedefs = this.NonVisitedLinedefs ()
            let linedef = Tracer.FindClosestLinedef linedefs

            let firstTracer =
                { this with
                    endVertex = if linedef.FrontSidedef.IsSome then linedef.Start else linedef.End
                    currentVertex = Vector2.Zero
                    linedefs = linedefs
                    path = [linedef] }.Visit linedef

            let rec f (tracer: Tracer) =
                if tracer.NonVisitedLinedefs()
                match tracer.TryVisitNextLinedef () with
                | tracer, true -> f tracer
                | tracer, _ -> 
                    if tracer.IsFinished then 
                    tracer.NonVisitedLinedefs ()

        static member Create (linedefs: Linedef seq) =
            let tracer = {
                endVertex = Vector2.Zero
                currentVertex = Vector2.Zero
                linedefs = linedefs |> List.ofSeq
                visitedLinedefs = ImmutableHashSet<Linedef>.Empty
                path = [] }

            tracer.StartTrace ()

[<RequireQualifiedAccess>]
module PolygonFinder =
    module Polygon =
        let ofLinedefs (sides: Linedef seq) =
            let vertices =
                sides
                |> Seq.map (fun x -> 
                    if x.FrontSidedef.IsSome
                    then x.Start
                    else x.End) 
                |> Array.ofSeq

            Polygon.create vertices.[..vertices.Length - 2]

    let tryFindPolygon (linedefs: Linedef list) =
        let rec f (tracer: Tracer) =
            match tracer.TryVisitNextLinedef () with
            | tracer, true -> f tracer
            | tracer, _ ->
                if tracer.IsPathFinished
                then 
                    let poly =
                        match tracer.Path with
                        | [] -> None
                        | [_;_] -> None
                        | path -> Some (Polygon.ofLinedefs path)
                    poly,
                    tracer.RemainingLinedefs
                else None, tracer.RemainingLinedefs

        f (Tracer.Create linedefs)

    let find sector =
        let rec f (polygons: Polygon list) = function
            | [] -> polygons
            | linedefs -> 
                match tryFindPolygon linedefs with
                | Some poly, linedefs -> 
                    let linedefsInPolygon, linedefs =
                        linedefs
                        |> List.partition (fun x ->
                            Polygon.isPointInside x.Start poly &&
                            Polygon.isPointInside x.End poly)

                    let poly = Polygon.addChildren (f [] linedefsInPolygon) poly
                
                    f (poly :: polygons) linedefs
                | _, sides -> f polygons sides
        
        let linedefs =
            sector.Linedefs
            |> Seq.filter (fun x -> not (x.FrontSidedef.IsSome && x.BackSidedef.IsSome))
            |> Seq.distinctBy (fun x -> x.Start, x.End)
            |> List.ofSeq
        f [] linedefs

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Sector =
    let polygons sector = PolygonFinder.find sector
