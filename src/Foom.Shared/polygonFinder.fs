// Ideas from Pascal vd Heiden's Doom Builder 2 C# triangulation code.
[<RequireQualifiedAccess>]
module Foom.Shared.PolygonFinder

open System.Numerics
open System.Collections.Immutable

module Polygon =
    let ofLinedefs (sides: Linedef seq) =
        let vertices =
            sides
            |> Seq.map (fun x -> 
                if x.FrontSidedef.IsSome
                then x.Start
                else x.End) 
            |> Seq.distinct
            |> Array.ofSeq

        { Vertices = vertices; Children = [] }

type Tracer =
    private { 
        baseLinedef: Linedef
        currentVertex: Vector2
        linedefs: Linedef list
        visitedLinedefs: ImmutableHashSet<Linedef>
        path: Linedef list }

    static member private FindVertex (linedefs: Linedef list) =
        let s = linedefs |> List.minBy (fun x -> x.Start.X)
        let e = linedefs |> List.minBy (fun x -> x.End.X)

        if s.Start.X <= e.End.X 
        then s.Start
        else e.End

    static member private FindSideByVertex (v: Vector2) (linedefs: Linedef list) =
        match linedefs |> List.tryFind (fun x -> x.Start.Equals v) with
        | None -> linedefs |> List.find (fun x -> x.End.Equals v)
        | Some side -> side

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

    member this.TryVisitNextPath () =
        let head = this.path.Head
        let currentVertex = this.currentVertex
        let visitedLinedefs = this.visitedLinedefs

        match
            this.linedefs
            |> List.filter (fun l -> 
                match l.FrontSidedef.IsSome with
                | true -> currentVertex.Equals l.Start && not (visitedLinedefs.Contains l)
                | _ ->    currentVertex.Equals l.End && not (visitedLinedefs.Contains l)) with
        | [] -> this, false
        | [path] -> this.Visit path, true
        | paths ->
            let v1 =
                if head.FrontSidedef.IsSome
                then head.End - head.Start
                else head.Start - head.End

            let path =
                paths
                |> List.minBy (fun x ->
                    let v2 =
                        if x.FrontSidedef.IsSome
                        then x.End - x.Start
                        else x.Start - x.End

                    Vector3.Cross(Vector3 (v1, 0.f), Vector3 (v2, 0.f)).Z)
            this.Visit path, true

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
        let baseLinedef = Tracer.FindSideByVertex (Tracer.FindVertex linedefs) linedefs

        { baseLinedef = baseLinedef
          currentVertex = Vector2.Zero
          linedefs = linedefs
          visitedLinedefs = ImmutableHashSet<Linedef>.Empty
          path = [baseLinedef] }.Visit baseLinedef

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
            match tracer.TryVisitNextPath () with
            | tracer, true -> f tracer
            | tracer, _ -> None, tracer.RemainingLinedefs

    Tracer.Create linedefs
    |> f

let findPolygons sector =
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

                let poly = { poly with Children = f [] linedefsInPolygon }
            
                f (poly :: polygons) linedefs
            | _, sides -> f polygons sides
    
    let linedefs =
        sector.Linedefs
        |> Seq.distinctBy (fun x -> x.Start, x.End)
        |> List.ofSeq
        |> List.filter (fun x -> not (x.FrontSidedef.IsSome && x.BackSidedef.IsSome))
    f [] linedefs
