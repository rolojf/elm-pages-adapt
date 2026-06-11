---
name: static-pages
description: Create pre-rendered (static, build-time) pages in elm-pages - routes built with RouteBuilder.single/preRender, generating page lists with Glob, reading markdown/frontmatter content files. Use when building blogs, docs, marketing pages, or any content that does not change per-request. Not for per-request dynamic pages (server-rendering skill) or logins (sessions-auth skill).
---

# elm-pages: Pre-rendered (static) pages

Pre-rendered routes are resolved at build time (`elm-pages build`) into static HTML +
`content.dat` files in `dist/`. No server is needed to host them. `data` runs at
build-time, never in the browser. A `FatalError` in a pre-rendered route fails the
build (this is a feature: bad data never goes live).

**Scaffolding (`elm-pages run AddRoute ...`) is broken — create Route Module files
manually** using the templates below.

## Template: static route without RouteParams

`app/Route/About.elm` → `/about`:

```elm
module Route.About exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Head
import Html
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { body : String }


type alias ActionData =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single { data = data, head = head }
        |> RouteBuilder.buildNoState { view = view }


data : BackendTask FatalError Data
data =
    BackendTask.succeed { body = "Hello" }


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view : App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view app shared =
    { title = "About"
    , body = [ Html.text app.data.body ]
    }
```

## Template: dynamic segment, pages from content files

`app/Route/Blog/Slug_.elm` → `/blog/:slug`. With RouteParams, use
`RouteBuilder.preRender` and supply `pages` (the full list of params to build):

```elm
type alias RouteParams =
    { slug : String }


type alias Data =
    { title : String, body : String }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender { data = data, pages = pages, head = head }
        |> RouteBuilder.buildNoState { view = view }


pages : BackendTask FatalError (List RouteParams)
pages =
    Glob.succeed RouteParams
        |> Glob.match (Glob.literal "content/blog/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toBackendTask


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    BackendTask.File.bodyWithFrontmatter
        (\body ->
            Decode.map (\title -> { title = title, body = body })
                (Decode.field "title" Decode.string)
        )
        ("content/blog/" ++ routeParams.slug ++ ".md")
        |> BackendTask.allowFatal
```

Imports: `BackendTask.File`, `BackendTask.Glob as Glob`, `Json.Decode as Decode`.

Note the signature difference: with `single`, `data : BackendTask FatalError Data`;
with `preRender`, `data : RouteParams -> BackendTask FatalError Data`.

Keep `pages` and index listings in sync by sharing one helper BackendTask (e.g. a
`blogPosts` function used both by `pages` here and by the blog index route's `data`).

## Reading content files

```elm
BackendTask.File.rawFile : String -> BackendTask { fatal : FatalError, recoverable : FileReadError d } String
BackendTask.File.jsonFile : Decoder a -> String -> BackendTask { fatal : FatalError, recoverable : FileReadError Error } a
BackendTask.File.onlyFrontmatter : Decoder fm -> String -> BackendTask { fatal : FatalError, recoverable : FileReadError Error } fm
BackendTask.File.bodyWithFrontmatter : (String -> Decoder fm) -> String -> BackendTask { fatal : FatalError, recoverable : FileReadError Error } fm
BackendTask.File.bodyWithoutFrontmatter : String -> BackendTask { fatal : FatalError, recoverable : FileReadError d } String
```

Paths are relative to the project root (where `elm.json` is). Pipe through
`BackendTask.allowFatal` to convert recoverable errors into a build failure.

## Listing files with Glob

```elm
Glob.succeed (\filePath slug -> { filePath = filePath, slug = slug })
    |> Glob.captureFilePath
    |> Glob.match (Glob.literal "content/blog/")
    |> Glob.capture Glob.wildcard      -- captured → becomes a constructor argument
    |> Glob.match (Glob.literal ".md") -- matched but not captured
    |> Glob.toBackendTask
```

Other matchers: `Glob.digits`, `Glob.int`, `Glob.recursiveWildcard` (`**`,
`List String`), `Glob.oneOf`, `Glob.expectUniqueMatch` (fails unless exactly one file).

## Interactive static pages (local state)

Static pages can still have client-side TEA state. Use `buildWithLocalState`:

```elm
route =
    RouteBuilder.preRender { data = data, pages = pages, head = head }
        |> RouteBuilder.buildWithLocalState
            { view = view          -- view app shared model
            , init = init          -- init app shared = ( Model, Effect Msg )
            , update = update      -- update app shared msg model = ( Model, Effect Msg )
            , subscriptions = \routeParams path shared model -> Sub.none
            }
```

`Model`/`Msg` become real types; `route : RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg`.
In `view`, wrap messages: `Html.button [ onClick (PagesMsg.fromMsg Increment) ] [...]`.
The server renders with `init`'s model only (`update` never runs server-side), so the
initial view must look correct before hydration.

## Gotchas

- Pre-rendered routes have no access to `Server.Request` — no headers, no cookies, no
  query params at build time. If you need those, the route must be server-rendered.
- Flags from `index.ts` arrive only after hydration. `Pages.Flags` is
  `BrowserFlags Json.Decode.Value | PreRenderFlags` — you must handle `PreRenderFlags`
  with a default. Don't base layout on window dimensions from flags (causes a flash);
  use CSS media queries (e.g. `.responsive-mobile` / `.responsive-desktop` classes
  hidden by `@media` rules) instead.
- URLs for routes derive from the module path only; `pages` decides *which* param
  values exist, not the URL shape.
- `head` runs at build time per page — see the `seo-api-routes` skill for `Head.Seo`.
