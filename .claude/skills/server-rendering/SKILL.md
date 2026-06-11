---
name: server-rendering
description: Create server-rendered (request-time, dynamic) routes in elm-pages - RouteBuilder.serverRender, reading the incoming Server.Request (headers, query params, body), building Server.Response (render, redirects, JSON, status codes), and custom error pages via ErrorPage.elm. Use for pages whose content depends on the request, dashboards, or unlimited/CMS-driven URLs. For cookies/sessions/logins also load sessions-auth; for form submissions also load forms.
---

# elm-pages: Server-rendered routes

Server-rendered routes resolve `data` per request on the server. They receive the
incoming `Server.Request.Request` and return a `Server.Response.Response`, which can
render the page, redirect, render an error page, or return a raw body. Deploying them
requires an adapter (see `deployment` skill). `data` still never runs in the browser.

## Template

```elm
module Route.Profile exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Head
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App)
import Server.Request exposing (Request)
import Server.Response as Response exposing (Response)
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias Data =
    { username : String }


type alias ActionData =
    {}


route =
    RouteBuilder.serverRender { data = data, action = action, head = head }
        |> RouteBuilder.buildNoState { view = view }


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data routeParams request =
    case Server.Request.queryParam "user" request of
        Just username ->
            Response.render { username = username }
                |> BackendTask.succeed

        Nothing ->
            Response.errorPage ErrorPage.NotFound
                |> BackendTask.succeed


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action routeParams request =
    Response.render {}
        |> BackendTask.succeed
```

`serverRender` requires `action` even if unused (return `Response.render {}`).
`head` and `view` are the same as in static routes. Use `buildWithLocalState` for
client-side TEA state (same shape as in the `static-pages` skill).

## Server.Request — reading the request

```elm
method      : Request -> Method           -- Get | Post | ... | NonStandard String
header      : String -> Request -> Maybe String
headers     : Request -> Dict String String
queryParam  : String -> Request -> Maybe String
queryParams : Request -> Dict String (List String)
cookie      : String -> Request -> Maybe String
cookies     : Request -> Dict String String
body        : Request -> Maybe String
jsonBody    : Decoder value -> Request -> Maybe (Result Json.Decode.Error value)
rawFormData : Request -> Maybe (List ( String, String ))
rawUrl      : Request -> String
requestTime : Request -> Time.Posix
matchesContentType : String -> Request -> Bool
```

For parsed/validated forms use `Server.Request.formData` (see `forms` skill).

## Server.Response — building the response

```elm
render            : data -> Response data error        -- render this route's view
errorPage         : errorPage -> Response data errorPage
temporaryRedirect : String -> Response data error      -- 302
permanentRedirect : String -> Response data error      -- 308
json              : Json.Encode.Value -> Response data error
plainText         : String -> Response data error
emptyBody         : Response data error

withHeader          : String -> String -> Response data error -> Response data error
withStatusCode      : Int -> Response data Never -> Response data Never
withSetCookieHeader : SetCookie -> Response data error -> Response data error
```

Rule of thumb: if you can resolve `Data`, use `render`; if you can't, use `errorPage`
(expected failures) or let a `FatalError` propagate (unexpected failures → 500).

## Error pages: `app/ErrorPage.elm`

Must expose an `ErrorPage` type. Define variants per error case, carrying whatever
data the error view needs:

```elm
type ErrorPage
    = NotFound
    | InternalError String
    | PaywallAccessError { resource : PaidResource, planStatus : PlanStatus }
```

Render one from `data`/`action` with `Response.errorPage ErrorPage.NotFound`. You can
resolve BackendTasks first (e.g. look up why access failed) and pass the result into
the variant — short-circuiting the route without resolving its `Data`.

`ErrorPage.elm` has its own self-contained TEA (`init : ErrorPage -> ( Model, Effect Msg )`,
`update`, `view : ErrorPage -> Model -> View Msg`), so error pages can be interactive.

### FatalError vs ErrorPage

- `FatalError` in a server-rendered route renders `ErrorPage.internalError` with a
  context `String`. These strings can be low-level (e.g. HTTP failures passed through
  `BackendTask.allowFatal`) — log them, don't show them to end users in production.
- `FatalError` in a pre-rendered route fails the build instead.
- Use `Response.errorPage` when you have meaningful context to show; use `FatalError`
  for "should not happen" cases.

## Pattern: 404 from a data lookup

```elm
data routeParams request =
    fetchPost routeParams.slug          -- BackendTask FatalError (Maybe Post)
        |> BackendTask.map
            (\maybePost ->
                case maybePost of
                    Just post ->
                        Response.render { post = post }

                    Nothing ->
                        Response.errorPage ErrorPage.NotFound
            )
```

This is how CMS/database-driven routes serve unlimited URLs: any slug is accepted by
the router; `data` decides between rendering and 404.

## Gotchas

- Server-side rendering calls `init` then `view` — never `update`. The first paint
  must be correct from `init` alone.
- `withStatusCode` only works on `Response data Never` (no error page possible).
- Redirect responses short-circuit rendering; prefer them over rendering a "please
  log in" page when unauthenticated (see `sessions-auth`).
- API-style raw responses (`json`, `plainText`) from a Route Module are possible, but
  dedicated endpoints belong in `app/Api.elm` (see `seo-api-routes` skill).
