module Application

open System

module TelegramBot =
    module C = CommandParser

    let private parseFreeResult =
        function
        | Regex "Mem: +(\\d+) +(\\d+)" [ total; used ] -> sprintf "%i MB / %i MB" (int used / 1024) (int total / 1024)
        | _ -> "ERROR: Can't get free memory"

    let private makeCommands runBash =
        [ C.Command [ C.Name "free"
                      C.Description "Show free memory"
                      C.Return(lazy (runBash ("free", null) |> Async.map parseFreeResult)) ]
          C.Command [ C.Name "cat"
                      C.Param (C.StringParam "path")
                      C.OnCallback (fun path -> runBash ("cat", path)) ] ]

    let main ownerUserId readMessage sendMessage runBash =
        async {
            let eval userId msg =
                if ownerUserId = userId then
                    C.eval msg (makeCommands runBash)
                else
                    Error "Not authorized access"

            while true do
                let! (user, msg) = readMessage
                let! response = eval user msg |> Result.fold id async.Return
                do! sendMessage user response
        }

module Bash =
    open System.Diagnostics

    let run (cmd: string, arg: string) : string Async =
        async {
            let p =
                new Process(StartInfo = new ProcessStartInfo(FileName = cmd, Arguments = arg, RedirectStandardOutput = true))

            p.Start() |> ignore
            p.WaitForExit()
            return p.StandardOutput.ReadToEnd()
        }
        |> Async.catch
        |> Async.map (function Ok x -> x | Error e -> string e)

type HealthStatus =
    | NoneHealth
    | HealthStarting
    | Healthy
    | Unhealthy

type Status =
    | Exited
    | Running of TimeSpan * HealthStatus
    | Created

type ContainerId = ContainerId of string

type State =
    { containers: Map<ContainerId, Status>
      allContainers: Map<ContainerId, Status>
      nextRun: bool }
    static member Empty =
        { containers = Map.empty
          allContainers = Map.empty
          nextRun = false }

module Service =
    let parseStatus state status =
        let parseUpTime =
            function
            | Regex "Up About an hour" [] -> Ok <| TimeSpan.FromHours 1.0
            | Regex "Up Less than a second" [] -> Ok <| TimeSpan.FromSeconds 1.0
            | Regex "Up About a minute" [] -> Ok <| TimeSpan.FromMinutes 1.0
            | Regex "Up (\d+) second" [ x ] -> Ok <| TimeSpan.FromSeconds(float x)
            | Regex "Up (\d+) minute" [ x ] -> Ok <| TimeSpan.FromMinutes(float x)
            | Regex "Up (\d+) hours" [ x ] -> Ok <| TimeSpan.FromHours(float x)
            | Regex "Up (\d+) day" [ x ] -> Ok <| TimeSpan.FromDays(float x)
            | Regex "Up (\d+) week" [ x ] -> Ok <| TimeSpan.FromDays(7.0 * float x)
            | Regex "Up (\d+) month" [ x ] -> Ok <| TimeSpan.FromDays(30.0 * float x)
            | time -> Error <| sprintf "Can't parse '%s'" time

        let parseHealth =
            function
            | Regex """\(health: starting\)""" [] -> HealthStarting
            | Regex """\(healthy\)""" [] -> Healthy
            | Regex """\(unhealthy\)""" [] -> Unhealthy
            | _ -> NoneHealth

        match state with
        | "exited" -> Ok Exited
        | "running" ->
            parseUpTime status
            |> Result.map (fun s -> Running(s, parseHealth status))
        | "created" -> Ok Created
        | s -> Error <| sprintf "Can't parse '%s'" s

    let private getUpdateMessages state containers =
        let isExited =
            function
            | Exited -> true
            | Running _
            | Created -> false

        let isRestarted id status =
            match status, Map.tryFind id state.containers with
            | Running (time, _), Some (Running (oldTime, _)) when time < oldTime -> true
            | _ -> false

        let messages =
            containers
            |> List.filter (fun (id, _, s) -> isExited s && Map.containsKey id state.containers)
            |> List.map (fun (_, name, _) -> sprintf "Service <%s> crashed" name)

        let restartMessages =
            containers
            |> List.filter (fun (id, _, s) -> isRestarted id s)
            |> List.map (fun (_, name, _) -> sprintf "Service <%s> restarted" name)

        let unhealthyMessages =
            containers
            |> List.filter
                (fun (id, _, s) ->
                    match s, Map.tryFind id state.containers with
                    | Running (_, Unhealthy), Some (Running (_, Healthy)) -> true
                    | Running (_, Unhealthy), Some (Running (_, HealthStarting)) -> true
                    | _ -> false)
            |> List.map (fun (_, name, _) -> sprintf "Service <%s> unhealthy" name)

        let newContainersMessages =
            if not state.nextRun then
                []
            else
                containers
                |> List.choose
                    (fun (id, name, _) ->
                        match Map.tryFind id state.allContainers with
                        | None -> Some <| sprintf "New container <%s> appear" name
                        | Some _ -> None)

        { state with
              nextRun = true
              allContainers =
                  containers
                  |> List.map (fun (id, _, status) -> id, status)
                  |> Map.ofList
              containers =
                  containers
                  |> List.choose
                      (fun (id, _, status) ->
                          if isExited status then
                              None
                          else
                              Some(id, status))
                  |> Map.ofList },

        List.concat [ restartMessages
                      messages
                      unhealthyMessages
                      newContainersMessages ]

    let run getContainers sendMessages (state : _ ref) =
        async {
            let! containers = getContainers
            let (state', messages) = getUpdateMessages state.Value containers
            state.Value <- state'
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
                |> Seq.map
                    (fun x ->
                        let status =
                            Service.parseStatus x.State x.Status
                            |> Result.unwrap

                        ContainerId x.ID, toNames x.Names, status)
                |> Seq.toList

            use client =
                (new DockerClientConfiguration()).CreateClient()

            return
                client
                    .Containers
                    .ListContainersAsync(
                        ContainersListParameters(Limit = !>100L)
                    )
                    .Result
                |> mapContainers
        }
        |> Async.catch
        |> Async.map (
            function
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

        async {
            return!
                channel.Reader.ReadAsync().AsTask()
                |> Async.AwaitTask
        }

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
        |> List.map
            (fun text ->
                t.client.SendTextMessageAsync(!>user, text)
                |> Async.AwaitTask
                |> Async.Catch)
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
                  (Telegram.mkMessageReader telegram
                   |> Async.map (fun x -> x.user, x.message))
                  (fun userId msg -> sendMessages userId [ msg ])
                  Bash.run ]
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously

    0
