module Tests

open Xunit
open Swensen.Unquote
open Application

module T = TestFramework

[<Fact>]
let ``send notification when service unhealthy from started`` () =
    TestFramework.run
    <| fun env ->
        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 HealthStarting ]
        env.run ()
        test <@ [] = !env.messages @>

        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 Unhealthy ]
        env.run ()
        test <@ [ "Service <service1> unhealthy" ] = !env.messages @>

[<Fact>]
let ``send notification when service unhealthy`` () =
    TestFramework.run
    <| fun env ->
        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 Healthy ]
        env.run ()
        test <@ [] = !env.messages @>

        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 Unhealthy ]
        env.run ()
        test <@ [ "Service <service1> unhealthy" ] = !env.messages @>

[<Fact>]
let ``no double unhealthy notification`` () =
    TestFramework.run
    <| fun env ->
        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 Unhealthy ]
        env.run ()
        test <@ [] = !env.messages @>

        env.setContainers [ ContainerId "1", "service1", T.mkRunning' 10.0 Unhealthy ]
        env.run ()
        test <@ [] = !env.messages @>

[<Fact>]
let ``send notification when service restarted`` () =
    TestFramework.run
    <| fun env ->
        env.setContainers [ ContainerId "1", "service1", T.mkRunning 30.0 ]
        env.run ()
        Assert.Equal(box [], !env.messages)

        env.setContainers [ ContainerId "1", "service1", T.mkRunning 10.0 ]
        env.run ()
        Assert.Equal(box [ "Service <service1> restarted" ], !env.messages)

[<Fact>]
let ``same call should not trigger`` () =
    TestFramework.run
        (fun env ->
            env.run ()
            Assert.Equal(box [], !env.messages)

            env.setContainers [ ContainerId "1", "service1", T.mkRunning 0.0
                                ContainerId "2", "service2", Exited
                                ContainerId "3", "service3", Created ]

            for _ in 1 .. 3 do
                env.run ()
                Assert.Equal(box [], !env.messages))

[<Fact>]
let ``first start test`` () =
    TestFramework.run
        (fun env ->
            env.run ()
            Assert.Equal(box [], !env.messages)

            env.setContainers [ ContainerId "1", "service1", T.mkRunning 0.0
                                ContainerId "2", "service2", Exited
                                ContainerId "3", "service3", Exited ]

            env.run ()
            Assert.Equal(box [], !env.messages))

[<Fact>]
let ``integration test`` () =
    TestFramework.run
        (fun env ->
            env.setContainers [ ContainerId "1", "service1", T.mkRunning 0.0
                                ContainerId "2", "service2", T.mkRunning 0.0
                                ContainerId "3", "service3", T.mkRunning 0.0 ]

            env.run ()
            Assert.Equal(box [], !env.messages)

            env.setContainers [ ContainerId "1", "service1", T.mkRunning 0.0
                                ContainerId "2", "service2", Exited
                                ContainerId "3", "service3", Exited ]

            env.run ()

            Assert.Equal(
                box [ "Service <service2> crashed"
                      "Service <service3> crashed" ],
                !env.messages
            ))
