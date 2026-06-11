---
name: backend-tasks
description: The BackendTask data layer of elm-pages - composing tasks (map/andThen/combine), HTTP requests, reading files and globs, environment variables, calling Node via BackendTask.Custom, do-notation, streams, FatalError handling. Use whenever fetching or producing data in route `data`/`action` functions or in scripts.
---

# elm-pages: BackendTask

`BackendTask error value` is the only data-fetching primitive. It runs at build-time
(pre-rendered routes), request-time (server-rendered routes), or script run-time —
never in the browser. Secrets used inside a BackendTask stay server-side unless put
into the route's `Data`.

## Core composition

```elm
succeed : a -> BackendTask error a
fail    : error -> BackendTask error a
fromResult : Result error value -> BackendTask error value

map     : (a -> b) -> BackendTask error a -> BackendTask error b
map2 .. map9
andThen : (a -> BackendTask error b) -> BackendTask error a -> BackendTask error b
andMap  : BackendTask error a -> BackendTask error (a -> b) -> BackendTask error b

combine  : List (BackendTask error v) -> BackendTask error (List v)  -- PARALLEL
sequence : List (BackendTask error v) -> BackendTask error (List v)  -- sequential
```

Prefer `combine`/`map2..9` (parallel) over `andThen`/`sequence` unless a task needs
the previous result or its side effects.

## Error handling and FatalError

Fallible tasks have error type `{ fatal : FatalError, recoverable : SomeError }`:

- `BackendTask.allowFatal` — discard the recoverable detail, crash with a nice
  message (build failure on pre-rendered routes; 500 on server-rendered; non-zero
  exit in scripts). The usual choice.
- `BackendTask.onError` / `mapError` / `toResult` — recover or inspect the error.
- `FatalError.fromString "msg"` / `FatalError.build { title, body }` — create your own.
- A task typed `BackendTask Never a` is proven unable to fail.

```elm
BackendTask.Http.getJson url decoder      -- BackendTask { fatal, recoverable : Http.Error } a
    |> BackendTask.allowFatal             -- BackendTask FatalError a
```

## HTTP — `BackendTask.Http`

```elm
get     : String -> Expect a -> BackendTask { fatal : FatalError, recoverable : Error } a
getJson : String -> Decoder a -> BackendTask { fatal : FatalError, recoverable : Error } a
post    : String -> Body -> Expect a -> BackendTask { fatal : FatalError, recoverable : Error } a
request : { url, method, headers, body, retries, timeoutInMs } -> Expect a -> ...
getWithOptions : { url, expect, headers, cacheStrategy, retries, timeoutInMs, cachePath } -> ...

-- Expect: expectJson, expectString, expectBytes, expectWhatever, withMetadata
-- Body: emptyBody, jsonBody, stringBody mimeType content, bytesBody, multipartBody
-- Error: BadUrl | Timeout | NetworkError | BadStatus Metadata String | BadBody _ _
```

Build-time HTTP responses are cached; control with `cacheStrategy`
(`IgnoreCache | ForceRevalidate | ForceReload | ForceCache | ErrorUnlessCached`).

## Files — `BackendTask.File`

```elm
rawFile, binaryFile, jsonFile, onlyFrontmatter, bodyWithFrontmatter,
bodyWithoutFrontmatter, exists, optional
```

Paths relative to the project root (next to `elm.json`). Details and frontmatter
examples in the `static-pages` skill.

## Globs — `BackendTask.Glob`

Pipeline builder: `Glob.succeed ctor |> Glob.match ... |> Glob.capture ... |>
Glob.toBackendTask`. `match` consumes without capturing; `capture` feeds the
constructor. Matchers: `literal`, `wildcard` (`*`), `recursiveWildcard` (`**`),
`digits`, `int`, `oneOf`. `expectUniqueMatch` fails unless exactly one file matches.

## Environment — `BackendTask.Env`

```elm
get    : String -> BackendTask error (Maybe String)            -- optional
expect : String -> BackendTask { fatal, recoverable : Error } String  -- required
```

## Node escape hatch — `BackendTask.Custom`

Only when no Elm-native BackendTask covers the need (e.g. database drivers):

```elm
BackendTask.Custom.run "functionName" (Encode.object [...]) decoder
-- : BackendTask { fatal : FatalError, recoverable : BackendTask.Custom.Error } b
```

Calls the export `functionName` from `custom-backend-task.ts` (or `.js`) — an async
function `(input) => result`, transpiled with esbuild. Keep this file minimal.

## Do-notation — `BackendTask.Do`

For imperative-feeling sequential pipelines (most useful in scripts):

```elm
import BackendTask.Do as Do

Do.env "API_KEY" <| \apiKey ->
Do.command "curl" [ url ] <| \output ->
Do.log ("Got: " ++ output) <| \() ->
BackendTask.succeed output
```

Helpers: `do`, `allowFatal`, `each`, `command` (capture stdout), `exec` (print),
`env`, `glob`, `log`, `failIf`, `noop`.

## Other modules

- `BackendTask.Random` — `generate : Random.Generator a -> BackendTask error a`, `int32`.
- `BackendTask.Time` — `now`, `zone`, `zoneByName`.
- Context wrappers on any task: `BackendTask.inDir "subdir"`, `BackendTask.quiet`,
  `BackendTask.withEnv key value`, `BackendTask.finally cleanup`.

## Streams — `BackendTask.Stream`

Composable Node streams for large data / piping processes:

```elm
import BackendTask.Stream as Stream

Stream.fileRead "data.json"
    |> Stream.pipe Stream.gzip
    |> Stream.pipe (Stream.fileWrite "data.json.gz")
    |> Stream.run
```

Sources: `stdin`, `fromString`, `fileRead`, `command cmd args`, `http {...}`.
Sinks: `stdout`, `stderr`, `fileWrite`. Transforms: `gzip`, `unzip`.
Consume: `run` (discard output), `read` (get `{ metadata, body : String }`),
`readJson decoder`. For shell commands, `commandWithOptions` supports
`allowNon0Status`, `withOutput` (stderr routing), `withTimeout ms`.

## Gotchas

- Don't hand-write JSON encoders/decoders for `Data` — the Lamdera compiler
  serializes it automatically.
- `BackendTask` is not Elm's `Task`: no browser APIs, parallel by default.
- In `data` for pre-rendered routes, every task runs at build time — a flaky HTTP
  dependency makes builds flaky; consider `retries` in `BackendTask.Http.request`.
