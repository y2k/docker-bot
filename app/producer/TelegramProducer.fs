module TelegramProducer

open Domain
open Newtonsoft.Json
open Suave
open Suave.Filters
open Suave.Operators
open System
open System.IO
open Telegram.Bot
open Telegram.Bot.Types

let produce (token: string) origin (dispatch: TelegramMessageReceived -> unit Async) =
    let client = TelegramBotClient token
    let hookPath = $"/webhook/{Guid.NewGuid()}"

    client
        .SetWebhookAsync(url = Uri(Uri(origin), hookPath).ToString(), maxConnections = 1, dropPendingUpdates = true)
        .Wait()

    choose
        [ POST
          >=> path hookPath
          >=> request (fun req ctx ->
              async {
                  try
                      use reader = new StreamReader(new MemoryStream(req.rawForm))
                      let serializer = JsonSerializer.Create()
                      let update = serializer.Deserialize(reader, typedefof<Update>) :?> Update

                      let userId = update.Message.From.Id |> string |> UserId
                      let message = update.Message.Text
                      let msg = TelegramMessageReceived(userId, message)
                      do! dispatch (msg)
                  with e ->
                      printfn "%O" e

                  return! Successful.NO_CONTENT ctx
              }) ]
    |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ] }
    |> snd
