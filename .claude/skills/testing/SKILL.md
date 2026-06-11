---
name: testing
description: Unit-testing elm-pages BackendTasks and Scripts with Test.BackendTask - simulating files, env vars, HTTP, commands, time, and asserting on results and side effects. Use when writing elm-test suites for data functions or scripts.
---

# elm-pages: Testing BackendTasks

`Test.BackendTask` runs BackendTasks inside elm-test with all I/O simulated — no
real files, network, or shell. The flow: configure a `TestSetup`, build a
`BackendTaskTest` from a task or script, simulate I/O responses in order, then
assert.

## Basic shape

```elm
import Test.BackendTask as TestBT
import Expect
import Test exposing (Test)


myTest : Test
myTest =
    myBackendTask                       -- BackendTask FatalError String
        |> TestBT.fromBackendTaskWith
            (TestBT.init
                |> TestBT.withFile "content/post.md" "# Hello"
                |> TestBT.withEnv "API_KEY" "test-key"
            )
        |> TestBT.simulateHttpGet "https://api.example.com/x" """{"ok":true}"""
        |> TestBT.expectSuccessWith (Expect.equal "expected result")
```

## Setup (`TestSetup`)

```elm
init : TestSetup
withFile       : path -> contents -> TestSetup -> TestSetup
withBinaryFile : path -> Bytes -> ...
withEnv        : name -> value -> ...
withTime       : Time.Posix -> ...
withRandomSeed : Int -> ...
withStdin      : String -> ...
withWhich      : command -> resolvedPath -> ...
```

Create the test with `fromBackendTask` / `fromBackendTaskWith setup` for a
`BackendTask FatalError a`, or `fromScript` / `fromScriptWith setup` for a whole
`Script`.

## Simulating I/O

Each simulate call answers the next matching effect the task performs:

```elm
simulateHttpGet  : url -> responseBody -> BackendTaskTest a -> BackendTaskTest a
simulateHttpPost : url -> responseBody -> ...
simulateHttp     : method -> url -> responseBody -> ...
simulateHttpError : url -> ...                       -- network failure
simulateCustom   : name -> input -> responseJson -> ...  -- BackendTask.Custom.run
simulateCommand  : commandString -> output -> ...        -- shell commands
simulateQuestion : answer -> ...                         -- Script.question
simulateReadKey  : key -> ...
```

## Assertions

```elm
expectSuccess     : BackendTaskTest a -> Test
expectSuccessWith : (a -> Expectation) -> BackendTaskTest a -> Test
expectFailure     : BackendTaskTest a -> Test
expectFailureWith : (FatalError -> Expectation) -> BackendTaskTest a -> Test

-- side-effect checks, chained before the expect*:
ensureFileWritten : path -> contents -> ...   -- Script.writeFile happened
ensureFileExists / ensureFile / ensureNoFile
ensureCommand : command -> args -> ...
ensureHttpGet : url -> ...
ensureStdout / ensureStderr : String -> ...
```

Time zones: `Test.BackendTask.Time` provides `utc`, `fixedOffsetZone minutes`, and
`withTimeZone : TimeZone -> TestSetup -> TestSetup` for testing
`BackendTask.Time.zone*`.

## Example: testing a script end to end

```elm
scriptTest : Test
scriptTest =
    TestBT.fromScriptWith
        (TestBT.init |> TestBT.withEnv "TOKEN" "abc")
        Deploy.run
        |> TestBT.simulateCommand "git rev-parse HEAD" "deadbeef\n"
        |> TestBT.simulateHttpPost "https://deploy.example.com" """{"id":1}"""
        |> TestBT.ensureStdout "Deployed deadbeef"
        |> TestBT.expectSuccess
```

## Gotchas

- Simulations are consumed in the order the task requests them; parallel tasks
  (`combine`, `map2`) may interleave — match simulations to actual request order.
- An unsimulated effect fails the test (`expectTestError` asserts that on purpose).
- This tests BackendTasks/Scripts only — not Route Module `view`/`update` (use
  elm-test/elm-program-test conventions for pure frontend logic).
