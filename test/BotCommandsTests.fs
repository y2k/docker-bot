module BotCommandsTests

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
        t.channel.Reader.ReadAsync().AsTask()
        |> Async.AwaitTask

module T = Application.TelegramBot

type TestFramework() =
    let inCommands : string AsyncQueue.t = AsyncQueue.make ()
    let outCommands : string AsyncQueue.t = AsyncQueue.make ()

    let freeShellReponse = ref ""

    do
        let mainAsync =
            T.main
                (Application.Telegram.userIdFrom "1")
                (async {
                    let! msg = AsyncQueue.pop inCommands
                    return (Application.Telegram.userIdFrom "1"), msg
                 })
                (fun _ msg -> AsyncQueue.push outCommands msg |> async.Return)
                (fun _ -> async.Return !freeShellReponse)

        mainAsync |> Async.Start

    member _.setFreeShellReponse response = freeShellReponse := response
    member _.send msg = AsyncQueue.push inCommands msg

    member _.assetMessage asset : unit =
        let lastError : exn option ref = ref None

        let rec test count =
            async {
                match AsyncQueue.tryRead outCommands with
                | Some msg ->
                    try
                        asset msg
                    with e ->
                        if count > 0 then
                            lastError := Some e
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

    env.setFreeShellReponse
        """
              total        used        free      shared  buff/cache   available
Mem:        2037088      775168      185660        1016     1076260     1051064
Swap:       2097148      189440     1907708"""

    env.send "/free"
    env.assetMessage (fun msg -> test <@ "757 MB / 1989 MB" = msg @>)
