module BotCommandsTests

open Domain
open Xunit
open Swensen.Unquote

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

type TestFramework() =
    let inputTelegramMessages: string AsyncQueue.t = AsyncQueue.make ()
    let outputTelegramMessages: string AsyncQueue.t = AsyncQueue.make ()

    let bashLastResponse = ref ""

    let handleCommands (commands: Command list) =
        for cmd in commands do
            match cmd with
            | :? WriteMsg as WriteMsg (_, msg) -> AsyncQueue.push outputTelegramMessages msg
            | :? TelegramBot.WriteRunBash as TelegramBot.WriteRunBash (_, _, _, f) ->
                AsyncQueue.push outputTelegramMessages (f bashLastResponse.Value)
            | _ -> ()

        async.Zero()

    let getTelegramMessage =
        async {
            let! msg = AsyncQueue.pop inputTelegramMessages
            return TelegramMessageReceived((Application.Telegram.userIdFrom "1"), msg)
        }

    do
        async {
            let! msg = getTelegramMessage
            let commands = TelegramBot.handleMessage (UserId "1") msg
            do! handleCommands commands
        }
        |> Async.Start

    member _.setBashLastResponse response = bashLastResponse.Value <- response

    member _.send msg =
        AsyncQueue.push inputTelegramMessages msg

    member _.assetMessage asset : unit =
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

        test 10 |> Async.RunSynchronously

[<Fact>]
let ``free command`` () =
    let env = TestFramework()

    env.setBashLastResponse
        """
              total        used        free      shared  buff/cache   available
Mem:        2037088      775168      185660        1016     1076260     1051064
Swap:       2097148      189440     1907708"""

    env.send "/free"
    env.assetMessage (fun msg -> test <@ "757 MB / 1989 MB" = msg @>)
