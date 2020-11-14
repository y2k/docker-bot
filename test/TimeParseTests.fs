module TimeParseTests

open System
open Xunit
open Swensen.Unquote
open Application

[<Fact>]
let ``parse time tests`` () =
    let test a b =
        let a =
            Service.parseStatus "running" a
            |> Result.bind (function Running x -> Ok x | _ -> failwith "???")
        test <@ Ok b = a @>

    test "Up Less than a second" <| TimeSpan.FromSeconds 1.0
    test "Up 59 seconds" <| TimeSpan.FromSeconds 59.0
    test "Up About a minute"<|  TimeSpan.FromMinutes 1.0
    test "Up 2 minutes" <| TimeSpan.FromMinutes 2.0
    test "Up 8 days" <| TimeSpan.FromDays 8.0
    test "Up 2 weeks" <| TimeSpan.FromDays 7.0 * 2.0
    test "Up 2 months" <| TimeSpan.FromDays 30.0 * 2.0
