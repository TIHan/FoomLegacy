﻿namespace Foom.Shared.UserCommand

open System.Collections.Immutable

type UserCommand =
    | MapZoomIn = 0
    | MapZoomOut = 1
    | BeginMapMove = 2
    | EndMapMove = 3

type UserCommandState =
    private { commands: UserCommand list }

    static member Default = { commands = [] }

    member this.Add (cmd: UserCommand) = { this with commands = cmd :: this.commands }

    member this.Commands = this.commands |> List.rev
     