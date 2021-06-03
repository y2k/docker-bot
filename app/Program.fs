module Application

open System

module TelegramBot =
    module C = CommandParser

    let private parseFreeResult =
        function
        | Regex "Mem: +(\\d+) +(\\d+)" [ total; used ] -> sprintf "%i MB / %i MB" (int used / 1024) (int total / 1024)
        | _ -> "ERROR: Can't get free memory"

    let private commands runBash =
        [ C.Command [ C.Name "free"
                      C.Description "Show free memory"
                      C.Return(lazy (runBash "free" |> Async.map parseFreeResult)) ] ]

    let main readMessage sendMessage runBash =
        async {
            let eval msg = C.eval msg (commands runBash)

            while true do
                let! (user, msg) = readMessage

                let! response =
                    match eval msg with
                    | Ok msg -> msg
                    | Error e -> async.Return e

                do! sendMessage user response
        }

    let shell (_: string) : string Async = TODO()

// === === === === === === === === === === === === === ===

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
    { containers: Map<ContainerId, Status> }
    static member Empty = { containers = Map.empty }

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

        { state with
              containers =
                  containers
                  |> List.choose
                      (fun (id, _, status) ->
                          if isExited status then
                              None
                          else
                              Some(id, status))
                  |> Map.ofList },
        restartMessages @ messages @ unhealthyMessages

    let run getContainers sendMessages state =
        async {
            let! containers = getContainers
            let (state', messages) = getUpdateMessages !state containers
            state := state'
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

module Telegram =
    open Telegram.Bot

    type t = private { client: TelegramBotClient }

    let make token = { client = TelegramBotClient token }

    let getNewMessage t = async { return TODO() }

    let sendMessage t (user: string) messages =
        messages
        |> List.map
            (fun text ->
                t.client.SendTextMessageAsync(!>user, text)
                |> Async.AwaitTask)
        |> Async.Sequential
        |> Async.Ignore

[<EntryPoint>]
let main argv =
    let telegram = Telegram.make argv.[0]
    let userId = argv.[1]
    let sendMessages = Telegram.sendMessage telegram

    printfn "Application started ..."

    let checkAsync =
        async {
            let state = ref State.Empty

            while true do
                do! Service.run Docker.getContainers (sendMessages userId) state
                do! Async.Sleep 15_000
        }

    [ checkAsync
      TelegramBot.main
          (Telegram.getNewMessage telegram)
          (fun userId msg -> sendMessages userId [ msg ])
          TelegramBot.shell ]
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously

    0
