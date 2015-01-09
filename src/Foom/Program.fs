﻿open System
open System.IO
open System.Drawing
open System.Diagnostics
open System.Numerics

open Foom.Client

// http://gafferongames.com/game-physics/fix-your-timestep/
module GameLoop =
    type private GameLoop<'T> = { 
        State: 'T
        PreviousState: 'T
        LastTime: int64
        UpdateTime: int64
        UpdateAccumulator: int64
        RenderAccumulator: int64
        RenderFrameCount: int
        RenderFrameCountTime: int64
        RenderFrameLastCount: int }

    let start (state: 'T) (pre: unit -> unit) (update: int64 -> int64 -> 'T -> 'T) (render: float32 -> 'T -> 'T -> unit) =
        let targetUpdateInterval = (1000. / 30.) * 10000. |> int64
        let targetRenderInterval = (1000. / 12000.) * 10000. |> int64
        let skip = (1000. / 5.) * 10000. |> int64

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.Ticks

        let rec loop gl =
            let currentTime = time ()
            let deltaTime =
                match currentTime - gl.LastTime with
                | x when x > skip -> skip
                | x -> x

            let updateAcc = gl.UpdateAccumulator + deltaTime

            // We do not want our render accumulator going out of control,
            // so let's put a limit of its interval.
            let renderAcc = 
                match gl.RenderAccumulator with
                | x when x > targetRenderInterval -> targetRenderInterval
                | x -> x + deltaTime

            let rec processUpdate gl =
                if gl.UpdateAccumulator >= targetUpdateInterval
                then
                    let state = update gl.UpdateTime targetUpdateInterval gl.State

                    processUpdate
                        { gl with 
                            State = state
                            PreviousState = gl.State
                            UpdateTime = gl.UpdateTime + targetUpdateInterval
                            UpdateAccumulator = gl.UpdateAccumulator - targetUpdateInterval }
                else
                    gl

            let processRender gl =
                if gl.RenderAccumulator >= targetRenderInterval then
                    render (single gl.UpdateAccumulator / single targetUpdateInterval) gl.PreviousState gl.State

                    let renderCount, renderCountTime, renderLastCount =
                        if currentTime >= gl.RenderFrameCountTime + (10000L * 1000L) then
                            printfn "%A" gl.RenderFrameLastCount
                            1, gl.RenderFrameCountTime + (10000L * 1000L), gl.RenderFrameCount
                        else
                            gl.RenderFrameCount + 1, gl.RenderFrameCountTime, gl.RenderFrameLastCount

                    { gl with 
                        LastTime = currentTime
                        RenderAccumulator = gl.RenderAccumulator - targetRenderInterval
                        RenderFrameCount = renderCount
                        RenderFrameCountTime = renderCountTime
                        RenderFrameLastCount = renderLastCount }
                else
                    { gl with LastTime = currentTime }

            pre ()
       
            { gl with UpdateAccumulator = updateAcc; RenderAccumulator = renderAcc }
            |> processUpdate
            |> processRender
            |> loop

        loop
            { State = state
              PreviousState = state
              LastTime = 0L
              UpdateTime = 0L
              UpdateAccumulator = targetUpdateInterval
              RenderAccumulator = 0L
              RenderFrameCount = 0
              RenderFrameCountTime = 0L
              RenderFrameLastCount = 0 }

open Foom.Client
open Foom.Shared.UserCommand

type GameState = {
    UserCommandState: UserCommandState
    Client: Client.ClientState }

[<EntryPoint>]
let main argv = 
    Runtime.GCSettings.LatencyMode <- Runtime.GCLatencyMode.Batch

    let inputEventsToClientCommands (input: InputState) (state: GameState) =
        let cmdState =
            input.Events
            |> List.fold (fun (cmdState: UserCommandState) evt ->
                match evt with

                | MouseWheelScrolled (_, x) ->
                    match x with
                    | x when x < 0 -> cmdState.SetCommand UserCommand.MapZoomIn
                    | x when x > 0 -> cmdState.SetCommand UserCommand.MapZoomOut
                    | _ -> cmdState

                | MouseButtonPressed MouseButtonType.Left -> cmdState.SetCommand UserCommand.MapMove
                | MouseButtonReleased MouseButtonType.Left -> cmdState.SetCommand UserCommand.MapMove

                | _ -> cmdState
            ) (state.UserCommandState.ClearCommands ())
        cmdState.SetMousePosition { X = input.Mouse.X; Y = input.Mouse.Y }

    GameLoop.start { UserCommandState = UserCommandState.Create ([UserCommand.MapMove]); Client = Client.init () }
        (fun () ->
            Input.pollEvents ()
        )
        (fun time interval curr ->
            GC.Collect ()

            let input = Input.getState ()

            let cmdState = inputEventsToClientCommands input curr

            let client = Client.update cmdState curr.Client
            
            { curr with UserCommandState = cmdState; Client = client }
        ) 
        (fun t prev curr ->
            Client.draw t prev.Client curr.Client
        )
    0
