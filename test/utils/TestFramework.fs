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

    { setContainers = (fun cs -> containers := cs)
      run =
          (fun _ ->
              Service.run (async { return !containers }) (fun ms -> async { messages := ms }) state
              |> Async.RunSynchronously)
      messages = messages }
    |> callback

let mkRunning timeSec =
    Running(TimeSpan.FromSeconds timeSec, NoneHealth)

let mkRunning' timeSec h =
    Running(TimeSpan.FromSeconds timeSec, h)
