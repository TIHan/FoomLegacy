namespace Foom.Shared.UserCommand

type UserCommand =
    | MapZoomIn = 0
    | MapZoomOut = 1
    | BeginMapMove = 2
    | EndMapMove = 3

type UserCommandState

type UserCommandState with

    static member Default : UserCommandState

    member Add : cmd: UserCommand -> UserCommandState

    member Commands : UserCommand list
