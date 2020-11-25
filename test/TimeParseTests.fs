module TimeParseTests

open System
open Xunit
open Swensen.Unquote
open Application

[<Theory>]
[<InlineData("Up About an hour", "01:00:00")>]
[<InlineData("Up Less than a second", "00:00:01")>]
[<InlineData("Up 1 second", "00:00:01")>]
[<InlineData("Up 59 seconds", "00:00:59")>]
[<InlineData("Up About a minute", "00:01:00")>]
[<InlineData("Up 2 minutes", "00:02:00")>]
[<InlineData("Up 8 days", "8.00:00:00")>]
[<InlineData("Up 2 weeks", "14.00:00:00")>]
[<InlineData("Up 2 months", "60.00:00:00")>]
let ``parse time tests`` input expected =
    let expected = TimeSpan.Parse expected

    let actual =
        Service.parseStatus "running" input
        |> Result.bind
            (function
            | Running x -> Ok x
            | _ -> failwith "???")

    test <@ Ok expected = actual @>
