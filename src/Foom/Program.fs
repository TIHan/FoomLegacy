open System
open System.IO
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

    let start (state: 'T) (pre: unit -> Async<unit>) (update: int64 -> int64 -> 'T -> Async<'T>) (render: float32 -> 'T -> 'T -> Async<unit>) = async {
        let targetUpdateInterval = (1000. / 30.) * 10000. |> int64
        let targetRenderInterval = (1000. / 12000.) * 10000. |> int64
        let skip = (1000. / 5.) * 10000. |> int64

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.Ticks

        let rec loop gl = async {
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

            let rec processUpdate gl = async {
                if gl.UpdateAccumulator >= targetUpdateInterval
                then
                    let! state = update gl.UpdateTime targetUpdateInterval gl.State

                    return! processUpdate
                        { gl with 
                            State = state
                            PreviousState = gl.State
                            UpdateTime = gl.UpdateTime + targetUpdateInterval
                            UpdateAccumulator = gl.UpdateAccumulator - targetUpdateInterval }
                else
                    return gl
            }

            let processRender gl = async {
                if gl.RenderAccumulator >= targetRenderInterval then
                    do! render (single gl.UpdateAccumulator / single targetUpdateInterval) gl.PreviousState gl.State

                    let renderCount, renderCountTime, renderLastCount =
                        if currentTime >= gl.RenderFrameCountTime + (10000L * 1000L) then
                            printfn "%A" gl.RenderFrameLastCount
                            1, gl.RenderFrameCountTime + (10000L * 1000L), gl.RenderFrameCount
                        else
                            gl.RenderFrameCount + 1, gl.RenderFrameCountTime, gl.RenderFrameLastCount

                    return 
                        { gl with 
                            LastTime = currentTime
                            RenderAccumulator = gl.RenderAccumulator - targetRenderInterval
                            RenderFrameCount = renderCount
                            RenderFrameCountTime = renderCountTime
                            RenderFrameLastCount = renderLastCount }
                else
                    return { gl with LastTime = currentTime }
            }

            do! pre ()
       
            let gl = { gl with UpdateAccumulator = updateAcc; RenderAccumulator = renderAcc }
            let! gl = processUpdate gl
            let! gl = processRender gl
            return! loop gl
        }

        return! loop
            { State = state
              PreviousState = state
              LastTime = 0L
              UpdateTime = 0L
              UpdateAccumulator = targetUpdateInterval
              RenderAccumulator = 0L
              RenderFrameCount = 0
              RenderFrameCountTime = 0L
              RenderFrameLastCount = 0 }
    }

open Foom.Client
open Foom.Shared.UserCommand

type GameState = {
    Client: Client.ClientState }

let inputStateToCommandAndMouseState (input: InputState) (state: GameState) =
    input.Events
    |> List.fold (fun (userCmd: UserCommandState) evt ->
        match evt with

        | MouseWheelScrolled (_, x) ->
            match x with
            | x when x < 0 -> userCmd.Add UserCommand.MapZoomIn
            | x when x > 0 -> userCmd.Add UserCommand.MapZoomOut
            | _ -> userCmd

        | MouseButtonPressed MouseButtonType.Left -> userCmd.Add UserCommand.BeginMapMove
        | MouseButtonReleased MouseButtonType.Left -> userCmd.Add UserCommand.EndMapMove

        | _ -> userCmd
    ) (UserCommandState.Default), input.Mouse

let startFoom () = async {
    let! client = Client.init ()

    let pre : unit -> Async<unit> = fun () -> async { Input.pollEvents () }

    let update : int64 -> int64 -> GameState -> Async<GameState> =
        fun time interval curr -> async {
            GC.Collect ()

            let input = Input.getState ()

            let userCmd, mouse = inputStateToCommandAndMouseState input curr

            let! client = Client.update userCmd mouse curr.Client
            
            return { curr with Client = client } 
        }

    let render : float32 -> GameState -> GameState -> Async<unit> = fun t prev curr -> Client.draw t prev.Client curr.Client

    do! GameLoop.start { Client = client } pre update render
}

[<EntryPoint>]
let main argv = 
    Runtime.GCSettings.LatencyMode <- Runtime.GCLatencyMode.Batch
    startFoom () |> Async.RunSynchronously
    0
