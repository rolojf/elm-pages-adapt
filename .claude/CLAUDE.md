# elm-pages — working instructions (this repo)

elm-pages (dillonkearns/elm-pages, v3+) has two uses: **websites** (pre-rendered and/or
server-rendered routes via file-based routing under `app/Route/`) and **scripts** (standalone
Elm programs run with `elm-pages run`).

## This repo

A **public template** (git remote `epa`) for Tailwind-styled, Cloudinary-backed sites deployed
on **Netlify** (previously AWS/S3). Downstream sites merge from it with
`git merge --no-edit epa/main --allow-unrelated-histories` (see README.md):
psm1, cfd3, solarpaq1, psolar, reftex1 (`github.com/rolojf/<name>`) — they may lag this repo.
`elm-pages` is pinned at exactly `3.5.1` (Elm pkg `dillonkearns/elm-pages` `12.3.0`). No test
suite. `elm-review` is installed but has no config directory.

## Working rules

- Write Elm. Avoid JS/TS unless unavoidable; the only JS escape hatches are `index.ts`
  (ports/flags) and `custom-backend-task.ts` (Node for `BackendTask.Custom.run`). Prefer
  built-in BackendTask modules (Http, File, Glob, Env, Stream) over custom JS.
- **Scaffolding is broken**: `elm-pages run AddRoute Foo.Bar_` does not work. Create Route
  Module files manually under `app/Route/` (templates in the `static-pages` /
  `server-rendering` skills).
- `elm-pages-reference.md` in this folder is the API source of truth — check it before writing.
- Verify with real output (compiler, dev server, script runs) rather than assuming.

## Toolchain & commands

Requires Node.js + the **Lamdera compiler** (Elm-compiler fork giving automatic binary
serialization — why `Data` needs no JSON codecs). Lamdera is an npm dev dep; if dependency
builds fail with a corrupt-cache note, run `lamdera reset`.

- `npm run elm` — dev server (`elm-pages dev`, port 1234). Tailwind rebuilds automatically.
- `npm run build` — production build → `dist/` (just `elm-pages build`; Tailwind v4 compiles
  inside Vite — there is **no** separate Tailwind step).
- `npm run server` — serve `dist/` at http://127.0.0.12:8421
- `npm run asdf` — deprecated S3 deploy.
- `npx elm-pages run <path-or-ModuleName>` — run a script.
- The `cli` package.json script points to a nonexistent path; use `npx elm-pages` instead.

## Conventions

- Parked/disabled routes are renamed `.old` / `.elm.out` (e.g. `Login.old`, `Temas.elm.out`)
  so the compiler skips them — don't delete or "fix" them. Live routes: `Index`, `About`,
  `Contacto`, `Blog/Slug_`, `Sub/Slug_`.
- Page content lives in `content/` (markdown via `src/MdConverter.elm`, HTML via
  `hecrj/html-parser`, plus `shared.yaml`). `HardCodedData.siteName` selects the content folder.

## Mental model

- Every route's `data : ... -> BackendTask FatalError Data` runs on the backend — at build-time
  (`RouteBuilder.preRender`) or request-time (`RouteBuilder.serverRender`). **BackendTasks never
  run in the browser**; secrets stay server-side unless put in `Data`.
- Lifecycle: resolve `data` → server-render HTML (`init` + `view`, never `update`) → browser
  hydrates → standard TEA → form submissions run the route's `action` server-side and re-render.
- Pre-rendered routes need no server (static `dist/`); server-rendered routes get a
  `Server.Request.Request` and need a hosting adapter.
- `Effect msg` (frontend, wraps Cmd) and `BackendTask` (backend) are unrelated concepts.

## Project structure

```
app/
├── Route/           -- Route Modules = the routes (file-based routing)
├── View.elm         -- exposes `View msg` (custom, see below) and `map`
├── Shared.elm       -- Model, Msg; site-wide layout + persistent state
├── Effect.elm       -- `Effect msg` + `perform`; Effect.fromCmd escape hatch
├── Site.elm         -- `config : SiteConfig` (global head tags, canonical URL)
├── Api.elm          -- ApiRoute definitions (RSS, sitemap, JSON endpoints)
└── ErrorPage.elm    -- `ErrorPage` type (404/500/custom)
src/                 -- own (non-framework) Elm modules
public/              -- static assets copied verbatim into dist/
index.ts             -- JS entrypoint: default export { load, flags }; imports style.css
style.css            -- Tailwind v4 entry (Vite-processed) — see Styling
elm-pages.config.mjs -- vite config (incl. @tailwindcss/vite), headTagsTemplate, adapter
custom-backend-task.ts -- Node functions for BackendTask.Custom (only if unavoidable)
script/              -- separate Elm project for elm-pages scripts
```

No user-defined `Main.elm`; elm-pages generates the entrypoint.

## Routing

File → URL: `Index.elm`→`/`, `OurTeam.elm`→`/our-team`, `Blog/Slug_.elm`→`/blog/:slug`,
`Docs/Section__.elm`→`/docs` + `/docs/:section`, `City/SPLAT_.elm`→`/city/...` (1+),
`SPLAT__.elm`→`/...` (0+). CamelCase module → kebab URL; trailing `_` = dynamic, `__` =
optional, splats only as final segment. (Full details in `05-file-based-routing.md`.)

Every Route Module exposes `ActionData, Data, Model, Msg, route`. Builder start: `single`
(static, no params) / `preRender` (static, needs `pages`) / `serverRender` (dynamic, needs
`action`); end: `buildNoState` / `buildWithLocalState` / `buildWithSharedState`. Wrap route
msgs in `view` with `PagesMsg.fromMsg`. Full templates in the `static-pages` /
`server-rendering` skills.

## Project specifics

### Custom `View` type (`app/View.elm`)
Adds a required `withMenu` field:
```elm
type alias View msg =
    { title : String, body : List (Html msg), withMenu : MenuInfo }  -- NoMenu | SiMenu (List Liga)
```
Every route sets `withMenu`: `NoMenu` for no nav, `SiMenu links` to populate it.

### Custom `Effect` type (`app/Effect.elm`)
Beyond standard `Cmd`: `EsperaPues Float msg` (delayed msg / setTimeout), `SoloAccedeLiga String
(Result Http.Error String -> msg)` (fire a GET), `PushUrl String` (navigate), `Enfoca msg String`
(focus a DOM element).

### Site constants (`src/HardCodedData.elm`)
All hardcoded site constants: `canonicalUrl`, `siteName` (content folder), image config
(`logoTrans`, `logoResource`), and locale. **Edit this file when setting up a new site instance.**

### Images via Cloudinary
`MiCloudinary.url transforms asset`, e.g. `MiCloudinary.url "f_auto,q_auto,w_800" "v123/img.jpg"`.

### Styling — Tailwind CSS v4
Integrated into Vite via `@tailwindcss/vite` (in `elm-pages.config.mjs` plugins); Tailwind builds
and hot-reloads inside `elm-pages dev` / `build` — **no** separate compile step (`npm run tail`,
`input.css`, `tailwind.css` are all gone). `style.css` is the single CSS entry, imported from
`index.ts` (`import "./style.css";`):
```css
@import "tailwindcss";
@import "./reto.css";
@config "./tailwind.config.cjs";
@source "./app/**/*.elm";
@source "./src/**/*.elm";
@source "./content/**/*.{html,md}";
```
`tailwind.config.cjs` (via `@config`) holds only the theme (`fontFamily`: Fira Sans / Roboto Slab)
and plugins (`@tailwindcss/typography`, `@tailwindcss/forms`); `aspect-ratio`/`autoprefixer`/
`postcss` were removed (native/built-in in v4). Classes are authored in Elm as `"tw <classes>"`
strings; in v4 the `tw ` prefix is only a marker (the bare `tw` token matches no utility), and
**a misspelled utility silently produces no CSS** — there is no build-time validation.

### Analytics (`src/Analytics.elm`)
`Analytics.eventoXReportar "event-name"` builds an `Event`; `Analytics.toEffect` converts it,
firing a GET to `/api-v2/<event>.json` via `Effect.SoloAccedeLiga`. (The `/api-v2` backend that
logged these is currently absent.)

### Shared state (`app/Shared.elm`)
Tracks `UsuarioSt` (`Desconocido | Rechazado | Conocido`), menu toggle, and fires analytics.
`template.onPageChange = Just OnPageChange` (the starter uses `Nothing`).

### Deployment
Netlify (adapter `elm-pages/adapter/netlify.js`); Netlify Functions in `functions/`, output in
`dist/`. Deprecated S3 deploy via `npm run asdf`.

## Which skill to load

| Task | Skill |
|---|---|
| Static/pre-rendered pages, blog/docs from files | `static-pages` |
| Dynamic per-request pages, Request/Response, error pages | `server-rendering` |
| Logins, protected pages, sessions, cookies | `sessions-auth` |
| Forms, user input, `action`, validation | `forms` |
| Data fetching: HTTP, files, glob, env, custom, streams | `backend-tasks` |
| CLI scripts, `elm-pages run`, bundling, local DB | `scripts` |
| Meta tags, OpenGraph, RSS/sitemap, manifest, API endpoints | `seo-api-routes` |
| Hosting, adapters, Netlify, custom servers | `deployment` |
| Unit-testing BackendTasks | `testing` |
| Bundle-size optimization with `View.freeze` (experimental) | `frozen-views` |

`elm-pages-reference.md` is the full API reference (source of truth). Ignore the obsolete upgrade
guides and framework-internal docs (`tui-*`, taint-analysis, etc.) in this folder.

## Elm code navigation — use the `elr` MCP tools, not grep

Prefer `elr` (Elm LSP via MCP) over Grep/Read/Glob for Elm navigation/editing — faster and exact:
/`elm_definition` (where defined), `elm_references` (usages), `elm_symbols` (list symbols),
`elm_diagnostics` (errors), `elm_format`, and the `elm_rename_*` / `elm_move_*` /
`elm_*_variant` refactors. **After editing any `.elm` file, run `elm_diagnostics` and fix errors
before continuing.** Fall back to Grep only for non-Elm text (comments, strings, config).
