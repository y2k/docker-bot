module Application

open Domain

module TelegramBot =
    module T = TelegramBot

    let main ownerUserId readMessage sendMessage runBash =
        async {
            while true do
                let! (user, msg) = readMessage

                let! response =
                    match T.eval ownerUserId user msg with
                    | :? T.WriteRunBash as T.WriteRunBash (c, p, f) -> runBash (c, p) |> Async.map f
                    | :? WriteMsg as WriteMsg msg -> async.Return msg
                    | _ -> async.Return null

                if not (isNull response) then
                    do! sendMessage user response
        }

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

module Service =
    let run getContainers sendMessages (state: _ ref) =
        async {
            let! containers = getContainers
            let effects = Service.getUpdateMessages state.Value containers

            for e in effects do
                match e with
                | :? State as state' -> state.Value <- state'
                | _ -> ()

            let messages =
                effects
                |> List.choose (function
                    | :? WriteMsg as WriteMsg msg -> Some msg
                    | _ -> None)

            do! sendMessages messages
        }

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
    type UserId = private UserId of string

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

[<EntryPoint>]
let main argv =
    async {
        let telegram = Telegram.make argv.[0]
        let userId = argv.[1] |> Telegram.userIdFrom
        let sendMessages = Telegram.sendMessage telegram

        do! Telegram.clearHistory telegram

        printfn "Application started ..."

        do!
            [ async {
                  let state = ref State.Empty

                  while true do
                      do! Service.run Docker.getContainers (sendMessages userId) state
                      do! Async.Sleep 15_000
              }
              TelegramBot.main
                  userId
                  (Telegram.mkMessageReader telegram |> Async.map (fun x -> x.user, x.message))
                  (fun userId msg -> sendMessages userId [ msg ])
                  Bash.run ]
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously

    0
