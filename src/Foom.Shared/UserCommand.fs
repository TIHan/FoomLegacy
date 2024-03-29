﻿namespace Foom.Shared.UserCommand

type UserCommand =
    | MapZoomIn
    | MapZoomOut
    | BeginMapMove
    | EndMapMove

type UserCommandState =
    { commands: UserCommand list }

    static member Default = { commands = [] }

    member this.Add (cmd: UserCommand) = { this with commands = cmd :: this.commands }

    member this.Commands = this.commands |> List.rev
     