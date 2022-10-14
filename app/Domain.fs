namespace Domain

open System

type Command =
    interface
    end

type Message =
    interface
    end

type UserId = UserId of string

type TelegramMessageReceived =
    | TelegramMessageReceived of user: UserId * message: string

    interface Message

type WriteMsg =
    | WriteMsg of UserId * string

    interface Command

type CallDelayCommand =
    | CallDelayCommand of TimeSpan * Message

    interface Command

module TelegramBot =
    type WriteRunBash =
        | WriteRunBash of user: UserId * bin: string * arg: string * f: (string -> string)

        interface Command

    module C = CommandParser

    let private parseFreeResult =
        function
        | Regex "Mem: +(\\d+) +(\\d+)" [ total; used ] -> sprintf "%i MB / %i MB" (int used / 1024) (int total / 1024)
        | _ -> "ERROR: Can't get free memory"

    let private makeCommands userId =
        [ C.Command
              [ C.Name "free"
                C.Description "Show free memory"
                C.Return(lazy (WriteRunBash(userId, "free", null, parseFreeResult) :> Command)) ]
          C.Command
              [ C.Name "cat"
                C.Param(C.StringParam "path")
                C.OnCallback(fun path -> WriteRunBash(userId, "cat", path, id)) ] ]

    let handleMessage ownerUserId (TelegramMessageReceived (userId, msg)) : Command list =
        if ownerUserId = userId then
            match C.eval msg (makeCommands userId) with
            | Ok r -> [ r ]
            | Error e -> [ WriteMsg(userId, e) ]
        else
            [ WriteMsg(userId, "Not authorized access") ]

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

    interface Command

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
        | "running" -> parseUpTime status |> Result.map (fun s -> Running(s, parseHealth status))
        | "created" -> Ok Created
        | s -> Error <| sprintf "Can't parse '%s'" s

    let private getUpdateMessages' ownerUserId state containers : Command list =
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
            |> List.filter (fun (id, _, s) ->
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
                |> List.choose (fun (id, name, _) ->
                    match Map.tryFind id state.allContainers with
                    | None -> Some <| sprintf "New container <%s> appear" name
                    | Some _ -> None)

        let stateUpdate =
            { state with
                nextRun = true
                allContainers = containers |> List.map (fun (id, _, status) -> id, status) |> Map.ofList
                containers =
                    containers
                    |> List.choose (fun (id, _, status) -> if isExited status then None else Some(id, status))
                    |> Map.ofList }

        let msgCmds =
            List.concat [ restartMessages; messages; unhealthyMessages; newContainersMessages ]
            |> List.map (fun x -> WriteMsg(ownerUserId, x) :> Command)

        stateUpdate :: msgCmds

    type CallbackMessage =
        | CallbackMessage

        interface Message

    let getUpdateMessages ownerUserId state containers (_: CallbackMessage) : Command list =
        [ CallDelayCommand(TimeSpan.FromSeconds 15, CallbackMessage)
          yield! getUpdateMessages' ownerUserId state containers ]
