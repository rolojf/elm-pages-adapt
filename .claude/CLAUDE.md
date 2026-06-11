# elm-pages — working instructions

elm-pages (dillonkearns/elm-pages, v3+) is an Elm framework with two distinct uses:

1. **Websites** — pre-rendered (static, build-time) and/or server-rendered (request-time)
   routes in the same app, defined by file-based routing under `app/Route/`.
2. **Scripts** — standalone Elm programs run outside the browser with `elm-pages run`.

## Working rules

- Write Elm. Avoid JavaScript/TypeScript unless there is no other way. The only JS
  escape hatches are `index.ts` (ports/flags) and `custom-backend-task.ts`
  (Node functions called via `BackendTask.Custom.run`). Prefer built-in BackendTask
  modules (Http, File, Glob, Env, Stream) before reaching for custom JS.
- **Scaffolding is broken**: `elm-pages run AddRoute Foo.Bar_` does NOT currently work.
  Create Route Module files manually under `app/Route/` (templates are in the
  `static-pages` and `server-rendering` skills).
- `elm-pages-reference.md` in this folder is the API source of truth. When unsure about
  a type signature or module API, check it before writing code.
- Verify with real output (compiler, dev server, script runs) rather than assuming.

## Toolchain & CLI

Requires Node.js and the **Lamdera compiler** (free Elm-compiler fork; provides automatic
binary serialization of Elm types — this is why `Data` needs no JSON encoders/decoders).
Lamdera is usually an npm dev dependency. If dependency builds fail with a corrupt-cache
note, run `lamdera reset`.

- `npx elm-pages init my-project` — new project
- `elm-pages dev` — dev server with hot reload (also reloads BackendTasks)
- `elm-pages build` — production build into `dist/`
- `npx elm-pages run <path-or-ModuleName>` — run a script

## Mental model

- Every route's `data : ... -> BackendTask FatalError Data` is resolved on the backend —
  at build-time (`RouteBuilder.preRender`) or at request-time (`RouteBuilder.serverRender`).
  **BackendTasks never run in the browser**; secrets used inside them stay server-side
  unless you put them in `Data`.
- Lifecycle: resolve `data` → server-render HTML (calls `init` + `view`, never `update`)
  → browser hydrates → client interactions are standard TEA → form submissions run the
  route's `action` BackendTask server-side and re-render.
- Pre-rendered routes need no server (static files from `dist/`). Server-rendered routes
  get a `Server.Request.Request` and need a hosting adapter.
- `Effect msg` (frontend, wraps Cmd) and `BackendTask` (backend) are unrelated concepts.

## Project structure

```
app/
├── Route/           -- Route Modules = the routes (file-based routing)
├── View.elm         -- must expose `View msg` (one type var) and `map`
├── Shared.elm       -- must expose Model, Msg; site-wide layout + persistent state
├── Effect.elm       -- must expose `Effect msg` + `perform`; Effect.fromCmd escape hatch
├── Site.elm         -- must expose `config : SiteConfig` (global head tags, canonical URL)
├── Api.elm          -- ApiRoute definitions (RSS, sitemap, JSON endpoints)
└── ErrorPage.elm    -- must expose `ErrorPage` type (404/500/custom)
src/                 -- your own (non-framework) Elm modules
public/              -- static assets copied verbatim into dist/
index.ts             -- JS entrypoint: default export { load, flags }
style.css            -- global CSS entrypoint (Vite-processed)
elm-pages.config.mjs -- vite config, headTagsTemplate, adapter
custom-backend-task.ts -- Node functions for BackendTask.Custom (only if unavoidable)
script/              -- separate Elm project for elm-pages scripts
```

There is no user-defined `Main.elm`; elm-pages generates the entrypoint.

## File-based routing

| File in `app/Route/` | URL | RouteParams |
|---|---|---|
| `Index.elm` | `/` | `{}` |
| `OurTeam.elm` | `/our-team` | `{}` |
| `Blog/Slug_.elm` | `/blog/:slug` | `{ slug : String }` |
| `Docs/Section__.elm` | `/docs` and `/docs/:section` | `{ section : Maybe String }` |
| `City/SPLAT_.elm` | `/city/...` (1+ segments) | `{ splat : ( String, List String ) }` |
| `SPLAT__.elm` | `/...` (0+ segments) | `{ splat : List String }` |

Static segments: `CapitalCamelCase` module → `kebab-case` URL. Dynamic: trailing `_`
(field name is the lowercased segment). Optional dynamic: `__`. Splats only as the
final segment.

## Route Module skeleton

Every Route Module must expose `ActionData, Data, Model, Msg, route`:

```elm
module Route.About exposing (ActionData, Data, Model, Msg, route)

type alias Model = {}
type alias Msg = ()
type alias RouteParams = {}
type alias Data = { ... }
type alias ActionData = {}

route : RouteBuilder.StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single { data = data, head = head }   -- no RouteParams
        |> RouteBuilder.buildNoState { view = view }
```

Builder start: `single` (static, no params) / `preRender` (static, needs `pages`) /
`serverRender` (dynamic, needs `action`). Builder end: `buildNoState` /
`buildWithLocalState` / `buildWithSharedState`. In `view`, wrap route msgs with
`PagesMsg.fromMsg`.

## Which skill to load

| Task | Skill |
|---|---|
| Static pages, blog/docs from files, pre-rendered routes | `static-pages` |
| Dynamic per-request pages, Request/Response, error pages | `server-rendering` |
| Logins, protected pages, sessions, cookies | `sessions-auth` |
| Forms, user input, `action`, validation | `forms` |
| Any data fetching: HTTP, files, glob, env, custom, streams | `backend-tasks` |
| CLI scripts, `elm-pages run`, bundling, local DB | `scripts` |
| Meta tags, OpenGraph, RSS/sitemap, PWA manifest, API endpoints | `seo-api-routes` |
| Hosting, adapters, Netlify, custom servers | `deployment` |
| Unit-testing BackendTasks | `testing` |
| Bundle-size optimization with `View.freeze` (experimental) | `frozen-views` |

## Docs in this folder

- `elm-pages-reference.md` — full API reference; the source of truth.
- Useful background: `04-file-structure.md`, `05-file-based-routing.md`,
  `13-error-pages.md`, `14-architecture.md`, `15-adapters.md`, `11-elm-pages-scripts.md`,
  `16-frozen-views.md`, `17-elm-pages-scripts-db.md`, `FAQ.md`.
- Ignore (framework-internals, not about using elm-pages): `frozen-views.md` (design doc),
  `tui-*.md`, `test-runner-review.md`, `elm-pages-taint-analysis-design.md`,
  `error-style-guide.md`.
- Ignore (obsolete versions): `upgrade-guide.md`, `migrating-from-1-to-2.md`,
  `7.0.0-elm-package-upgrade-guide.md`.
- Marketing-oriented intros (`01-what-is-elm-pages.md`, `03-philosophy.md`,
  `05-use-the-platform.md`) add nothing technical beyond the above.
