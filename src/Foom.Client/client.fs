[<RequireQualifiedAccess>]
module Foom.Client.Client

open System.Drawing
open System.Numerics

open Foom.Renderer
open Foom.Shared

let inline lerp x y t = x + (y - x) * t

type RendererState = {
    UniformColor: int<uniform>
    UniformProjection: int<uniform>
    Program: int<program>
    Application: Application
    Vbo: int
    VboLength: int
    Sectors: Polygon list [] }

type State = {
    Renderer: RendererState
    Level: Level
    ViewDistance: single
    ViewPosition: Vector3 }

// These are sectors to look for and test to ensure things are working as they should.
// 568 - map10 sunder
// 4 - map10  sunder
// 4371 - map14 sunder
// 28 - e1m1 doom
// 933 - map10 sunder
// 20 - map10 sunder
// 151 - map10 sunder
// 439 - map08 sunder
// 271 - map03 sunder

let init () =
    let wadFile = WadManager.openWad "sunder.wad"
    let lvl = WadManager.loadLevel "map10" wadFile
    let app = Renderer.init ()
    let vbo = Renderer.makeVbo ()
    let program = Backend.loadShaders ()
    let uniformColor = Renderer.getUniformColor program
    let uniformProjection = Renderer.getUniformProjection program

    let sectorPolygons =
        lvl.Sectors
        |> Array.mapi (fun i x -> 
            //if i = 151 then
            if true then 
                let poly = PolygonFinder.find x
                //printfn "POLYGONS: %A" poly.Length 
                poly
            else [])

    let vertices =
        match sectorPolygons with
        | [||] -> [||]
        | _ ->
            let vertexList =
                sectorPolygons
                |> Array.map (fun x ->
                    let vlist = 
                        x 
                        |> List.map (fun x -> x.Vertices) 
                    match vlist with
                    | [] -> [||]
                    | _ ->
                        vlist
                        |> List.reduce Array.append)

            match vertexList with
            | [||] -> [||]
            | _ ->
                vertexList
                |> Array.reduce Array.append

    Renderer.bufferVbo vertices (sizeof<Vector2> * vertices.Length) vbo

    let rendererState =
        { UniformColor = uniformColor
          UniformProjection = uniformProjection
          Program = program
          Application = app 
          Vbo = vbo
          VboLength = vertices.Length
          Sectors = sectorPolygons }
    
    { Renderer = rendererState
      Level = lvl
      ViewDistance = 1.f
      ViewPosition = Vector3 (-vertices.[0],0.f) }

let update (state: State) =
    Input.inputEvents
    |> Seq.fold (fun state e -> 
        match e with
        | MouseWheelScrolled (x, y) ->
            let viewDistance =
                match state.ViewDistance + single y / (2.f / state.ViewDistance) with
                | x when x <= 0.001f -> 0.001f
                | x when x >= 2.f -> 2.f
                | x -> x

            { state with 
                ViewDistance = viewDistance }
        | _ -> state) state

let draw t (prev: State) (curr: State) =
    Renderer.clear ()

    let projection = Matrix4x4.CreatePerspectiveFieldOfView (lerp prev.ViewDistance curr.ViewDistance t, (16.f / 9.f), 0.1f, 100.f) |> Matrix4x4.Transpose
    let model = Matrix4x4.CreateTranslation (lerp prev.ViewPosition curr.ViewPosition t) |> Matrix4x4.Transpose
    let mvp = (projection * model) |> Matrix4x4.Transpose

    Renderer.setUniformProjection curr.Renderer.UniformProjection mvp
    Renderer.setUniformColor curr.Renderer.UniformColor (RenderColor.OfColor Color.White)

    let index = ref 0

    curr.Renderer.Sectors
    |> Array.fold (fun count sector ->
        index := !index + 1
        match sector with
        | [] -> count
        | _ ->
            sector
            |> List.fold (fun count poly ->
                Renderer.drawArraysLoop count poly.Vertices.Length
                count + poly.Vertices.Length) count
    ) 0
    |> ignore

    Renderer.draw curr.Renderer.Application
