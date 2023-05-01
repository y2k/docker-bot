module TestFramework

open Domain
open System
open System.Threading

module AsyncQueue =
    open System.Threading.Channels

    type 't t = private { channel: Channel<'t> }

    let make () : 't t = { channel = Channel.CreateUnbounded() }

    let tryRead t =
        match t.channel.Reader.TryRead() with
        | true, x -> Some x
        | _ -> None

    let push t msg =
        t.channel.Writer.WriteAsync(msg) |> ignore

    let pop t =
        t.channel.Reader.ReadAsync().AsTask() |> Async.AwaitTask

type Env =
    { setContainers: (ContainerId * string * Status) list -> unit
      run: unit -> unit
      messages: string list ref
      setBashLastResponse: string -> unit
      send: string -> unit
      assetMessage: (string -> unit) -> unit }

let run (callback: Env -> unit) : unit =
    let state = ref State.Empty
    let messages = ref []
    let containers = ref []
    let inputTelegramMessages: string AsyncQueue.t = AsyncQueue.make ()
    let outputTelegramMessages: string AsyncQueue.t = AsyncQueue.make ()
    let bashLastResponse = ref ""

    let handleCommands (commands: Command list) =
        for cmd in commands do
            match cmd with
            | :? TelegramBot.WriteRunBash as TelegramBot.WriteRunBash (_, _, _, f) ->
                AsyncQueue.push outputTelegramMessages (f bashLastResponse.Value)
            | :? State as state' -> state.Value <- state'
            | :? WriteMsg as WriteMsg (_, msg) -> messages.Value <- messages.Value @ [ msg ]
            | _ -> ()

        async.Zero()

    let getTelegramMessage =
        async {
            let! msg = AsyncQueue.pop inputTelegramMessages
            return TelegramMessageReceived((Application.Telegram.userIdFrom "1"), msg)
        }

    let cts = new CancellationTokenSource()

    Async.Start(
        async {
            let! msg = getTelegramMessage
            let commands = TelegramBot.handleMessage (UserId "1") msg
            do! handleCommands commands
        },
        cts.Token
    )

    { setContainers = (fun cs -> containers.Value <- cs)
      run =
        (fun _ ->
            messages.Value <- []

            Service.handleMessage (UserId "0") state.Value containers.Value Service.CallbackMessage
            |> handleCommands
            |> Async.RunSynchronously)
      messages = messages
      setBashLastResponse = fun response -> bashLastResponse.Value <- response
      send = fun msg -> AsyncQueue.push inputTelegramMessages msg
      assetMessage =
        fun asset ->
            let lastError: exn option ref = ref None

            let rec test count =
                async {
                    match AsyncQueue.tryRead outputTelegramMessages with
                    | Some msg ->
                        try
                            asset msg
                        with e ->
                            if count > 0 then
                                lastError.Value <- Some e
                                do! Async.Sleep 100
                                do! test (count - 1)
                            else
                                raise e
                    | None ->
                        if count > 0 then
                            do! Async.Sleep 100
                            do! test (count - 1)
                        else
                            match !lastError with
                            | Some e -> raise e
                            | None -> failwith "No messages"
                }

            test 10 |> Async.RunSynchronously }
    |> callback

    cts.Cancel()

let mkRunning timeSec =
    Running(TimeSpan.FromSeconds timeSec, NoneHealth)

let mkRunning' timeSec h =
    Running(TimeSpan.FromSeconds timeSec, h)
