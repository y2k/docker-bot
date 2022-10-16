module BotCommandsTests

open TestFramework
open Xunit
open Swensen.Unquote

[<Fact>]
let ``free command`` () =
    run (fun env ->
        env.setBashLastResponse
            """
              total        used        free      shared  buff/cache   available
Mem:        2037088      775168      185660        1016     1076260     1051064
Swap:       2097148      189440     1907708"""

        env.send "/free"
        env.assetMessage (fun msg -> test <@ "757 MB / 1989 MB" = msg @>))
