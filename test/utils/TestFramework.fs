module TestFramework

open Domain
open System
open Application

type Env =
    { setContainers: (ContainerId * string * Status) list -> unit
      run: unit -> unit
      messages: string list ref }

let run (callback: Env -> unit) : unit =
    let state = ref State.Empty
    let messages = ref []
    let containers = ref []

    let handleCommands (commands: Command list) =
        for cmd in commands do
            match cmd with
            | :? State as state' -> state.Value <- state'
            | :? WriteMsg as WriteMsg (_, msg) -> messages.Value <- messages.Value @ [ msg ]
            | _ -> ()

        async.Zero()

    { setContainers = (fun cs -> containers.Value <- cs)
      run =
        (fun _ ->
            messages.Value <- []

            Service.getUpdateMessages (UserId "0") state.Value containers.Value Service.CallbackMessage
            |> handleCommands
            |> Async.RunSynchronously)
      messages = messages }
    |> callback

let mkRunning timeSec =
    Running(TimeSpan.FromSeconds timeSec, NoneHealth)

let mkRunning' timeSec h =
    Running(TimeSpan.FromSeconds timeSec, h)
