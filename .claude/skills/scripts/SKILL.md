---
name: scripts
description: Standalone Elm scripts run outside the browser with `elm-pages run` - Pages.Script API (file ops, shell commands, logging, prompts), CLI options parsing, machine-readable schemas, bundling to a single JS file, remote scripts from GitHub, spinners, and the experimental Pages.Db local database. Use when writing CLI tools, build scripts, code generators, or any non-website Elm program.
---

# elm-pages: Scripts (`elm-pages run`)

A script is an Elm module exposing `run : Script`, living in its own Elm project
(conventionally `script/` with its own `elm.json` that has `dillonkearns/elm-pages`
as a dependency). Scripts execute BackendTasks in Node — full access to files, HTTP,
env vars, shell commands (see `backend-tasks` skill for the data layer).

## Minimal script

`script/src/Hello.elm`:

```elm
module Hello exposing (run)

import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions
        (Script.log "Hello from elm-pages Scripts!")
```

Run it (module-name shorthand works for modules under `./script/src/`):

```shell
npx elm-pages run script/src/Hello.elm
npx elm-pages run Hello
```

Any path works as long as the module belongs to an Elm project with elm-pages
installed; the nearest `elm.json` is found automatically. New script folder from
scratch: `mkdir script && cd script && elm init && elm install dillonkearns/elm-pages`.

If a script resolves to a `FatalError`, it prints the error and exits non-zero.

## Pages.Script API

```elm
-- definition
withoutCliOptions : BackendTask FatalError () -> Script
withCliOptions    : Cli.Program.Config opts -> (opts -> BackendTask FatalError ()) -> Script
withSchema        : { description, cliOptions, encoder, run } -> Script

-- files
writeFile { path, body }   copyFile { from, to }   move { from, to }
removeFile   makeDirectory { recursive }   removeDirectory   makeTempDirectory

-- shell
command : String -> List String -> BackendTask FatalError String  -- capture stdout
exec    : String -> List String -> BackendTask FatalError ()      -- stream output
which / expectWhich : locate an executable

-- I/O
log : String -> BackendTask error ()
question : String -> BackendTask error String   -- prompt user
readKey  : BackendTask error String             -- single keypress
sleep    : Int -> BackendTask error ()
```

`BackendTask.Do` makes sequential scripts readable (`Do.exec`, `Do.command`,
`Do.log`, `Do.env`, `Do.glob` — see `backend-tasks` skill).

## CLI options (`dillonkearns/elm-cli-options-parser`)

```elm
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program


type alias CliOptions =
    { username : String, repo : String }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "username" |> Option.withDefault "dillonkearns")
                |> OptionsParser.with
                    (Option.optionalKeywordArg "repo" |> Option.withDefault "elm-pages")
            )


run : Script
run =
    Script.withCliOptions program
        (\{ username, repo } -> ...)
```

`--help` is generated automatically; unknown flags get typo suggestions.

## Machine-readable output (`Script.withSchema`)

For scripts consumed by tools/agents: define a `TsJson.Encode` encoder; the script
prints JSON, and `--introspect` (per script) or `npx elm-pages introspect` (all
scripts) emits the schema derived from the same encoder.

## Bundling

```shell
elm-pages bundle-script script/src/Stars.elm
```

Produces one minified JS file runnable in any Node environment (options parsing
included). `--optimize 0` none, `1` Elm compiler, `2` elm-optimize-level-2 (default).

## Remote scripts

Run scripts directly from GitHub repos or gists:

```shell
npx elm-pages run https://github.com/user/repo/blob/master/script/src/Stars.elm
npx elm-pages@3.0.12 run <url-pinned-to-commit>   # pin CLI version to match elm.json
npx elm-pages run https://gist.github.com/user/<gist-id>   # runs Main.elm
```

## Spinners (`Pages.Script.Spinner`)

```elm
Spinner.runTask "Fetching data..." someBackendTask

Spinner.steps myTask
    |> Spinner.withStep "Step 1" step1Fn
    |> Spinner.withStep "Step 2" step2Fn
    |> Spinner.runSteps
```

## Local database — `Pages.Db` (EXPERIMENTAL)

**Verify the installed elm-pages version actually ships `Pages.Db` before using;
this feature is not in the stable API reference.** Script-only (not for routes).
Requires `lamdera` on PATH. Persists an Elm type to a binary file with type-safe
migrations.

```elm
type alias Db = { count : Int }          -- schema in Db.elm

Pages.Db.default                          -- Connection to ./db.bin
Pages.Db.open (FilePath.fromString "...") -- custom path
Pages.Db.get connection                   -- BackendTask FatalError Db.Db
Pages.Db.update connection (\db -> ...)   -- BackendTask FatalError ()
Pages.Db.transaction connection (\db -> BackendTask ... ( Db.Db, a ))  -- locked
```

Workflow: `npx elm-pages db init` scaffolds `Db.elm` + `db/Db/Migrate/V1.elm` (seed).
After changing `Db.elm`: `npx elm-pages db migrate` (scaffolds `Migrate/VN.elm` with
`migrate : Db.VprevN.Db -> Db.Db` and `seed`), implement it, run `db migrate` again
to apply. `npx elm-pages db status` shows compatibility. Commit `Db.elm`,
`db/Db/V*.elm`, `db/Db/Migrate/*.elm`; gitignore `db.bin*`. Bundled scripts run
migrations automatically on end users' machines. Stuck lock: `rm -f db.bin.lock`.

## Gotchas

- A script module must expose exactly `run : Script`.
- `BackendTask.Custom.run` in a script uses the `custom-backend-task.ts` next to the
  script project's `elm.json`.
- `elm-pages run` auto-runs `elm-codegen install` if a `./codegen/` folder exists
  next to `./script/`.
- Scaffolding Route Modules via the AddRoute script is currently broken — don't
  build on it; create route files manually.
