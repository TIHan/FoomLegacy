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
            let rec f (paths: Linedef list list) (firstTracer: Tracer) (tracer: Tracer) =
                match tracer.TryVisitNextLinedef () with
                | tracer, true -> f paths firstTracer tracer
                | tracer, _ -> 
                    let isFinished = tracer.IsFinished
                    let tracer = if isFinished then tracer else firstTracer

                    let linedefs = tracer.NonVisitedLinedefs ()
                    if linedefs.Length > 0 then 
                        let paths = (tracer.path :: paths)
                        let newTracer = Tracer.Create linedefs
                        if isFinished then f (tracer.path :: paths) newTracer newTracer else f paths newTracer newTracer
                    else paths
                        
            f [] this this

        static member Create (linedefs: Linedef list) =
            let linedef = Tracer.FindClosestLinedef linedefs

            { endVertex = if linedef.FrontSidedef.IsSome then linedef.Start else linedef.End
              currentVertex = Vector2.Zero
              linedefs = linedefs
              visitedLinedefs = ImmutableHashSet<Linedef>.Empty
              path = [] }.Visit linedef

[<RequireQualifiedAccess>]
module PolygonFinder =
    module Polygon =
        let ofLinedefs (linedefs: Linedef list) =
            let vertices =
                linedefs
                |> List.map (fun x -> 
                    if x.FrontSidedef.IsSome
                    then x.Start
                    else x.End) 
                |> Array.ofList

            Polygon.create vertices.[..vertices.Length - 2]

    let find sector =        
        let linedefs =
            sector.Linedefs
            |> Seq.filter (fun x -> not (x.FrontSidedef.IsSome && x.BackSidedef.IsSome))
            |> Seq.distinctBy (fun x -> x.Start, x.End)
            |> List.ofSeq
        
        let tracer = Tracer.Create linedefs
        let paths = tracer.Run ()
        paths |> List.map (fun x -> Polygon.ofLinedefs x)

[<CompilationRepresentationAttribute (CompilationRepresentationFlags.ModuleSuffix)>]
module Sector =
    let polygons sector = PolygonFinder.find sector
