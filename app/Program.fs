module Application

open System

type Status =
    | Exited
    | Running of TimeSpan
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

        match state with
        | "exited" -> Ok Exited
        | "running" -> parseUpTime status |> Result.map Running
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
            | Running time, Some (Running oldTime) when time < oldTime -> true
            | _ -> false

        let messages =
            containers
            |> List.filter (fun (id, _, s) -> isExited s && Map.containsKey id state.containers)
            |> List.map (fun (_, name, _) -> sprintf "Service <%s> crashed" name)

        let restartMessages =
            containers
            |> List.filter (fun (id, _, s) -> isRestarted id s)
            |> List.map (fun (_, name, _) -> sprintf "Service <%s> restarted" name)

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
        restartMessages @ messages

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
                    .ListContainersAsync(ContainersListParameters(Limit = !>100L))
                    .Result
                |> mapContainers
        }

module Telegram =
    open Telegram.Bot

    let sendMessage token =
        let client = TelegramBotClient token

        fun (user: string) messages ->
            messages
            |> List.map
                (fun text ->
                    client.SendTextMessageAsync(!>user, text)
                    |> Async.AwaitTask)
            |> Async.Sequential
            |> Async.Ignore

[<EntryPoint>]
let main argv =
    let sendMessages = Telegram.sendMessage argv.[0] argv.[1]

    printfn "Application started ..."

    async {
        let state = ref State.Empty

        while true do
            do! Service.run Docker.getContainers sendMessages state
            do! Async.Sleep 15_000
    }
    |> Async.RunSynchronously

    0
