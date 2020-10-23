module Tests

open System
open Xunit
open Application

module TestFramework =
    type Env =
        { setContainers: (ContainerId * string * Status) list -> unit
          run: unit -> unit
          messages: string list ref }

    let run (callback: Env -> unit): unit =
        let state = ref State.Empty
        let messages = ref []
        let containers = ref []

        { setContainers = (fun cs -> containers := cs)
          run =
              (fun _ ->
                  Service.run (async { return !containers }) (fun ms -> async { messages := ms }) state
                  |> Async.RunSynchronously)
          messages = messages }
        |> callback

[<Fact>]
let ``send notification when service restarted`` () =
    TestFramework.run <| fun env ->
        env.setContainers [ ContainerId "1", "service1", Running <| TimeSpan.FromSeconds 30.0 ]
        env.run ()
        Assert.Equal(box [], !env.messages)

        env.setContainers [ ContainerId "1", "service1", Running <| TimeSpan.FromSeconds 10.0 ]
        env.run ()
        Assert.Equal (box [ "Service <service1> restarted" ], !env.messages)

[<Fact>]
let ``same call should not trigger`` () =
    TestFramework.run (fun env ->
        env.run ()
        Assert.Equal(box [], !env.messages)

        env.setContainers [ ContainerId "1", "service1", Running TimeSpan.Zero
                            ContainerId "2", "service2", Exited
                            ContainerId "3", "service3", Created ]
        for _ in 1 .. 3 do
            env.run ()
            Assert.Equal(box [], !env.messages))

[<Fact>]
let ``first start test`` () =
    TestFramework.run (fun env ->
        env.run ()
        Assert.Equal(box [], !env.messages)

        env.setContainers [ ContainerId "1", "service1", Running TimeSpan.Zero
                            ContainerId "2", "service2", Exited
                            ContainerId "3", "service3", Exited ]
        env.run ()
        Assert.Equal(box [], !env.messages))

[<Fact>]
let ``integration test`` () =
    TestFramework.run (fun env ->
        env.setContainers [ ContainerId "1", "service1", Running TimeSpan.Zero
                            ContainerId "2", "service2", Running TimeSpan.Zero
                            ContainerId "3", "service3", Running TimeSpan.Zero ]
        env.run ()
        Assert.Equal(box [], !env.messages)

        env.setContainers [ ContainerId "1", "service1", Running TimeSpan.Zero
                            ContainerId "2", "service2", Exited
                            ContainerId "3", "service3", Exited ]
        env.run ()
        Assert.Equal
            (box [ "Service <service2> crashed"
                   "Service <service3> crashed" ],
             !env.messages))
