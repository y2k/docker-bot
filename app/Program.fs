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
    open System
    open System.Runtime.InteropServices

    let private isMacOS = RuntimeInformation.IsOSPlatform(OSPlatform.OSX)

    let getContainers =
        async {
            let mapContainers (containers: #seq<ContainerListResponse>) =
                let toNames xs = Seq.reduce (sprintf "%s - %s") xs

                containers
                |> Seq.map (fun x ->
                    let status = Service.parseStatus x.State x.Status |> Result.unwrap

                    ContainerId x.ID, toNames x.Names, status)
                |> Seq.toList

            let client =
                if isMacOS then
                    let homeDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

                    (new DockerClientConfiguration(Uri $"unix://{homeDir}/.docker/run/docker.sock"))
                        .CreateClient()
                else
                    (new DockerClientConfiguration()).CreateClient()

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

    type t = private { client: TelegramBotClient }

    let make (token: string) = { client = TelegramBotClient token }

    let userIdFrom = UserId

    let sendMessage t (UserId user) messages =
        messages
        |> List.map (fun text -> t.client.SendTextMessageAsync(!>user, text) |> Async.AwaitTask |> Async.Catch)
        |> Async.Sequential
        |> Async.Ignore

let private handleCommands (state: State ref) sendMessage (dispatch: Message -> unit Async) (commands: Command list) =
    async {
        for cmd in commands do
            match cmd with
            | :? CallDelayCommand as CallDelayCommand (delay, cb) ->
                async {
                    do! Async.Sleep delay
                    do! dispatch cb
                }
                |> Async.Start
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
        [ async.Return(Service.handleMessage ownerUserId)
          |> Dsl.apply (async { return state.Value })
          |> Dsl.apply Docker.getContainers
          |> Dsl.wrapMsg
          async.Return(TelegramBot.handleMessage ownerUserId) |> Dsl.wrapMsg ]

[<EntryPoint>]
let main argv =
    async {
        let token = argv[0]
        let telegram = Telegram.make token
        let ownerUserId = argv[1] |> Telegram.userIdFrom
        let sendMessage = fun u m -> Telegram.sendMessage telegram u [ m ]

        let state = ref State.Empty
        let handleCommands = handleCommands state sendMessage

        printfn "Application started ..."

        let dispatch msg =
            Dsl.mkDispatchMessage (handleMessages ownerUserId state) handleCommands msg

        do!
            [ dispatch Service.CallbackMessage
              TelegramProducer.produce token argv[2] dispatch ]
            |> Async.Parallel
            |> Async.Ignore
    }
    |> Async.RunSynchronously

    0
