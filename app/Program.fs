module Application

type Status =
    | Exited
    | Running

type ContainerId = ContainerId of string

type State =
    { containers: ContainerId Set }
    static member Empty = { containers = Set.empty }

let inline (!>) (x: ^a): ^b =
    ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)

module Service =
    let parseStatus =
        function
        | "exited" -> Ok Exited
        | "running" -> Ok Running
        | s -> Error <| sprintf "Can't parse '%s'" s

    let private getUpdateMessages state containers =
        let messages =
            containers
            |> List.filter (fun (id, _, s) -> s = Exited && Set.contains id state.containers)
            |> List.map (fun (_, name, _) -> sprintf "Service <%O> is killed" name)

        { state with
              containers =
                  containers
                  |> List.map (fun (id, _, _) -> id)
                  |> Set.ofList },
        messages

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
                |> Seq.map (fun x ->
                    ContainerId x.ID,
                    toNames x.Names,
                    Service.parseStatus x.State
                    |> function
                    | Ok x -> x
                    | Error e -> failwith e)
                |> Seq.toList

            use client =
                (new DockerClientConfiguration()).CreateClient()

            return client.Containers.ListContainersAsync(ContainersListParameters(Limit = !>100L)).Result
                   |> mapContainers
        }

module Telegram =
    open Telegram.Bot

    let sendMessage token =
        let client = TelegramBotClient token
        fun (user: string) messages ->
            messages
            |> List.map (fun text ->
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
