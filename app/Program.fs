module Application

open Domain

module Bash =
    open System.Diagnostics

    let run (cmd: string, arg: string) : string Async =
        async {
            let p =
                new Process(
                    StartInfo = new ProcessStartInfo(FileName = cmd, Arguments = arg, RedirectStandardOutput = true)
                )

            p.Start() |> ignore
            p.WaitForExit()
            return p.StandardOutput.ReadToEnd()
        }
        |> Async.catch
        |> Async.map (function
            | Ok x -> x
            | Error e -> string e)

module Docker =
    open Docker.DotNet
    open Docker.DotNet.Models

    let getContainers =
        async {
            let mapContainers (containers: #seq<ContainerListResponse>) =
                let toNames xs = Seq.reduce (sprintf "%s - %s") xs

                containers
                |> Seq.map (fun x ->
                    let status = Service.parseStatus x.State x.Status |> Result.unwrap

                    ContainerId x.ID, toNames x.Names, status)
                |> Seq.toList

            use client = (new DockerClientConfiguration()).CreateClient()

            return
                client
                    .Containers
                    .ListContainersAsync(
                        ContainersListParameters(Limit = !> 100L)
                    )
                    .Result
                |> mapContainers
        }
        |> Async.catch
        |> Async.map (function
            | Ok x -> x
            | Error e ->
                printfn "%O" e
                [])

module Telegram =
    open Telegram.Bot
    open System.Threading
    open System.Threading.Channels
    open Telegram.Bot.Extensions.Polling

    type t = private { client: TelegramBotClient }

    let mkMessageReader { client = client } =
        let channel = Channel.CreateBounded(16)

        client.StartReceiving(
            { new IUpdateHandler with
                member _.get_AllowedUpdates() = [||]

                member _.HandleError(_, err, _) =
                    printfn "LOG :: %O" err
                    Tasks.Task.CompletedTask

                member _.HandleUpdate(_, update, _) =
                    channel
                        .Writer
                        .WriteAsync(
                            {| user = string update.Message.From.Id |> UserId
                               message = update.Message.Text |}
                        )
                        .AsTask() }
        )

        async { return! channel.Reader.ReadAsync().AsTask() |> Async.AwaitTask }

    let make token = { client = TelegramBotClient token }

    let clearHistory t =
        let rec clearHistory' offset =
            async {
                let! updates =
                    t.client.GetUpdatesAsync(offset = offset, limit = 100, timeout = 0)
                    |> Async.AwaitTask

                if not <| Array.isEmpty updates then
                    let lastMsg = updates |> Array.last
                    do! clearHistory' (lastMsg.Id + 1)
            }

        clearHistory' 0

    let userIdFrom = UserId

    let getNewMessage t =
        let rec tryRead offset =
            async {
                let! updates =
                    t.client.GetUpdatesAsync(offset = offset, limit = 1, timeout = 300)
                    |> Async.AwaitTask

                if Array.isEmpty updates then
                    return! tryRead offset
                else
                    return updates
            }

        let offset = ref 0

        async {
            let! updates = tryRead offset.Value
            let x = updates.[0]
            offset.Value <- x.Id + 1
            return string x.Message.From.Id |> UserId, x.Message.Text
        }

    let sendMessage t (UserId user) messages =
        messages
        |> List.map (fun text -> t.client.SendTextMessageAsync(!>user, text) |> Async.AwaitTask |> Async.Catch)
        |> Async.Sequential
        |> Async.Ignore

let private handleCommands (state: _ ref) sendMessage (dispatch: Message -> unit Async) (commands: Command seq) =
    async {
        for cmd in commands do
            match cmd with
            | :? CallDelayCommand as CallDelayCommand (delay, cb) ->
                do! Async.Sleep delay
                do! dispatch cb
            | :? State as state' -> state.Value <- state'
            | :? WriteMsg as WriteMsg (user, msg) -> do! sendMessage user msg
            | :? TelegramBot.WriteRunBash as TelegramBot.WriteRunBash (user, c, p, f) ->
                let! response = Bash.run (c, p) |> Async.map f
                do! sendMessage user response
            | _ -> ()
    }

module Dsl =
    let apply arg f =
        async {
            let! foo2 = f
            let! a2 = arg
            return foo2 a2
        }

    let wrapMsg (f: ('msg -> Command list) Async) (m: Message) =
        async {
            match m with
            | :? 'msg as msg ->
                let! f' = f
                return f' msg
            | _ -> return []
        }

    let build fs (msg: Message) =
        async {
            let! r = fs |> List.map (fun f -> f msg) |> Async.Parallel
            return List.concat r
        }

    let rec mkDispatchMessage handleMessages handleCommands (msg: Message) =
        async {
            let! commands = handleMessages msg
            do! handleCommands (mkDispatchMessage handleMessages handleCommands) commands
        }

let handleMessages ownerUserId (state: _ ref) =
    Dsl.build
        [ async.Return(Service.getUpdateMessages ownerUserId state.Value)
          |> Dsl.apply Docker.getContainers
          |> Dsl.wrapMsg
          async.Return(TelegramBot.handleMessage ownerUserId) |> Dsl.wrapMsg ]

[<EntryPoint>]
let main argv =
    async {
        let telegram = Telegram.make argv.[0]
        let ownerUserId = argv.[1] |> Telegram.userIdFrom
        let sendMessage = fun u m -> Telegram.sendMessage telegram u [ m ]

        do! Telegram.clearHistory telegram

        let state = ref State.Empty
        let handleCommands = handleCommands state sendMessage

        printfn "Application started ..."

        let dispatch msg =
            Dsl.mkDispatchMessage (handleMessages ownerUserId state) handleCommands msg

        do!
            [ dispatch Service.CallbackMessage
              async {
                  while true do
                      let! update = Telegram.mkMessageReader telegram
                      do! dispatch (TelegramMessageReceived(update.user, update.message))
              } ]
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously

    0
