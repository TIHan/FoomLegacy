namespace Foom.Shared.UserCommand

type UserCommand =
    | MapZoomIn
    | MapZoomOut
    | BeginMapMove
    | EndMapMove

type UserCommandState

type UserCommandState with

    static member Default : UserCommandState

    member Add : cmd: UserCommand -> UserCommandState

    member Commands : UserCommand list
