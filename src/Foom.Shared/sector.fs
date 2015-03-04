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
            baseLinedef: Linedef
            currentVertex: Vector2
            linedefs: Linedef list
            visitedLinedefs: ImmutableHashSet<Linedef>
            path: Linedef list }

        static member private FindMinVertex (linedefs: Linedef list) =
            let s = linedefs |> List.minBy (fun x -> x.Start.X)
            let e = linedefs |> List.minBy (fun x -> x.End.X)

            if s.Start.X <= e.End.X 
            then s.Start
            else e.End

        static member private FindLinedefByVertex (v: Vector2) (linedefs: Linedef list) =
            match linedefs |> List.tryFind (fun x -> x.Start.Equals v) with
            | None -> linedefs |> List.find (fun x -> x.End.Equals v)
            | Some linedef -> linedef

        member private this.Visit (linedef: Linedef) =
            { this with
                currentVertex = if linedef.FrontSidedef.IsSome then linedef.End else linedef.Start
                visitedLinedefs = this.visitedLinedefs.Add linedef
                path = linedef :: this.path }  

        member this.IsPathFinished =
            let s = this.baseLinedef
            let e = this.path.Head

            match s.FrontSidedef.IsSome, e.FrontSidedef.IsSome with
            | true, true -> s.Start.Equals e.End
            | true, false -> s.Start.Equals e.Start
            | false, true -> s.End.Equals e.End
            | false, false -> s.End.Equals e.Start    

        member this.TryVisitNextLinedef () =
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

        member this.RemainingLinedefs =
            if this.IsPathFinished
            then 
                this.linedefs 
                |> List.filter (fun x -> 
                    not (this.visitedLinedefs.Contains x))
            else
                this.linedefs
                |> List.filter (fun x -> not <| x.Equals this.baseLinedef)

        member this.Path = this.path  

        static member Create (linedefs: Linedef seq) =            
            let linedefs = linedefs |> List.ofSeq 
            let baseLinedef = Tracer.FindLinedefByVertex (Tracer.FindMinVertex linedefs) linedefs

            { baseLinedef = baseLinedef
              currentVertex = Vector2.Zero
              linedefs = linedefs
              visitedLinedefs = ImmutableHashSet<Linedef>.Empty
              path = [baseLinedef] }.Visit baseLinedef

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
            if tracer.IsPathFinished
            then 
                let poly =
                    match tracer.Path with
                    | [] -> None
                    | [_;_] -> None
                    | path -> Some (Polygon.ofLinedefs path)
                poly,
                tracer.RemainingLinedefs
            else
                match tracer.TryVisitNextLinedef () with
                | tracer, true -> f tracer
                | tracer, _ -> None, tracer.RemainingLinedefs

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
