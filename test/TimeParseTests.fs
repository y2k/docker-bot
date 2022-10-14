module TimeParseTests

open System
open Domain
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("Up About an hour", "01:00:00", "")>]
[<InlineData("Up Less than a second", "00:00:01", "")>]
[<InlineData("Up 1 second", "00:00:01", "")>]
[<InlineData("Up 59 seconds", "00:00:59", "")>]
[<InlineData("Up About a minute", "00:01:00", "")>]
[<InlineData("Up About a minute (health: starting)", "00:01:00", "starting")>]
[<InlineData("Up About a minute (healthy)", "00:01:00", "healthy")>]
[<InlineData("Up 2 minutes", "00:02:00", "")>]
[<InlineData("Up 2 hours", "02:00:00", "")>]
[<InlineData("Up 8 days", "8.00:00:00", "")>]
[<InlineData("Up 2 weeks", "14.00:00:00", "")>]
[<InlineData("Up 2 months", "60.00:00:00", "")>]
let ``parse time tests`` input expected textHealth =
    let health =
        match textHealth with
        | "starting" -> HealthStarting
        | "healthy" -> Healthy
        | _ -> NoneHealth
    let expected = Running (TimeSpan.Parse expected, health)
    let actual = Service.parseStatus "running" input |> Result.unwrap

    test <@ expected = actual @>
