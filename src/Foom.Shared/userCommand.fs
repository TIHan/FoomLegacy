namespace Foom.Shared.UserCommand

type UserCommand =
    | MapZoomIn = 0
    | MapZoomOut = 1
    | MapMove = 2

type MousePosition = { X: int; Y: int }

type UserCommandState =
    private { commands: UserCommand list
              toggledCommands: Map<UserCommand, bool>
              mousePosition: MousePosition }

    static member Create (cmdsToToggle: UserCommand list) =
        let toggledCommands =
            cmdsToToggle
            |> List.map (fun x -> x, false)
            |> Map.ofList
        { commands = []; toggledCommands = toggledCommands; mousePosition = { X = 0; Y = 0 } }

    member this.SetCommand (cmd: UserCommand) =
        match Map.tryFind cmd this.toggledCommands with
        | None -> 
            { this with commands = cmd :: this.commands }
        | Some isActive ->
            { this with toggledCommands = Map.add cmd (not isActive) this.toggledCommands }

    member this.MousePosition = this.mousePosition
    member this.SetMousePosition mousePosition = { this with mousePosition = mousePosition }

    member this.ClearCommands () = { this with commands = [] }

    member this.Commands = this.commands |> List.filter (fun x -> not (Map.containsKey x this.toggledCommands))
    member this.ToggledCommands = this.toggledCommands |> Map.filter (fun _ isActive -> isActive) |> Map.toList |> List.map (fun (x,_) -> x)