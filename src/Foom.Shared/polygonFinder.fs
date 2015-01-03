// Based off of Pascal vd Heiden's Doom Builder 2 C# triangulation code.
[<RequireQualifiedAccess>]
module Foom.Shared.PolygonFinder

open System
open System.Numerics
open System.Collections.Immutable

let pi = single Math.PI
let pi2 = pi * 2.f

let normalizeAngle a =
    let mutable a = a
    while a < 0.f do a <- a + pi2
    while a >= pi2 do a <- a - pi2
    a

let diffAngle a b =
    let mutable d = normalizeAngle a - normalizeAngle b
    if d < 0.f then d <- d + pi2
    if d > pi then d <- pi2 - d
    d

let sideOfLine (v1: Vector2) (v2: Vector2) (p: Vector2) =
    (p.Y - v1.Y) * (v2.X - v1.X) - (p.X - v1.X) * (v2.Y - v1.Y)

let calculateRelativeAngle (baseSide: Linedef) baseVertex (a: Linedef) (b: Linedef) =
    let ana =
        if a.End = baseVertex
        then a.Angle + pi
        else a.Angle

    let anb =
        if b.End = baseVertex
        then b.Angle + pi
        else b.Angle
    
    let mutable n = diffAngle ana anb

    let va =
        if a.Start = baseVertex
        then a.End
        else a.Start

    let vb =
        if b.Start = baseVertex
        then b.End
        else b.Start

    let dir =
        if baseSide.End = baseVertex
        then not baseSide.FrontSidedef.IsSome
        else baseSide.FrontSidedef.IsSome

    let s = sideOfLine va vb baseVertex
    if s < 0.f && dir then n <- pi2 - n
    if s > 0.f && not dir then n <- pi2 - n

    n

let compareAngle baseSide baseVertex x y =
    if x = y
    then 0
    else

    let ax = calculateRelativeAngle baseSide baseVertex baseSide x
    let ay = calculateRelativeAngle baseSide baseVertex baseSide y

    Math.Sign(ay - ax)

module Polygon =
    let ofLinedefs (sides: Linedef seq) =
        let vertices =
            sides
            |> Seq.map (fun x -> 
                if x.FrontSidedef.IsSome
                then x.Start
                else x.End) 
            |> Array.ofSeq

        { Vertices = vertices.[..vertices.Length - 2]; Children = [] }

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
                (currentVertex.Equals l.Start && not (visitedLinedefs.Contains l)) ||
                (currentVertex.Equals l.End && not (visitedLinedefs.Contains l))) with
        | [] -> this, false
        | [path] -> this.Visit path, true
        | paths ->
            let p =
                if head.FrontSidedef.IsSome
                then head.End
                else head.Start

            let paths =
                paths
                |> List.sortWith (fun x y ->
                    compareAngle head p x y)
            let path = paths.Head

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
        |> Seq.distinctBy (fun x -> x.Start, x.End)
        |> List.ofSeq
        |> List.filter (fun x -> not (x.FrontSidedef.IsSome && x.BackSidedef.IsSome))
    f [] linedefs
