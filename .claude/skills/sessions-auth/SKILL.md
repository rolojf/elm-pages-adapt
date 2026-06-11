---
name: sessions-auth
description: Sessions, cookies, logins, and protected pages in elm-pages - Server.Session (withSession, flash messages), Server.SetCookie options, auth redirect patterns. Use when restricting access to pages, implementing login/logout, or storing per-user state across requests. Requires server-rendered routes (load server-rendering skill too). Not needed for static sites.
---

# elm-pages: Sessions, cookies, auth

Sessions are signed cookies managed by `Server.Session`. They only exist on
server-rendered routes (`RouteBuilder.serverRender`) because they need the incoming
`Request`. Session contents are key–value `String` pairs, readable/writable only
server-side.

## Server.Session API

```elm
type Session  -- opaque

empty   : Session
get     : String -> Session -> Maybe String
insert  : String -> String -> Session -> Session
remove  : String -> Session -> Session
update  : String -> (Maybe String -> Maybe String) -> Session -> Session
withFlash : String -> String -> Session -> Session  -- value visible on next request only

withSession :
    { name : String
    , secrets : BackendTask error (List String)
    , options : Maybe SetCookie.Options
    }
    -> (Session -> BackendTask error ( Session, Response data errorPage ))
    -> Request
    -> BackendTask error (Response data errorPage)

withSessionResult :  -- like withSession, but distinguishes missing/invalid cookie
    ... -> (Result NotLoadedReason Session -> BackendTask error ( Session, Response data errorPage )) -> ...

type NotLoadedReason = NoSessionCookie | InvalidSessionCookie
```

`secrets` signs the session cookie. The first secret signs new sessions; older entries
still validate (lets you rotate secrets). Keep it in an env var. Your handler returns
the (possibly updated) session plus the response; elm-pages sets the cookie for you.

## Pattern: protected page

```elm
import BackendTask exposing (BackendTask)
import BackendTask.Env
import Server.Request exposing (Request)
import Server.Response as Response
import Server.Session


sessionConfig =
    { name = "session"
    , secrets =
        BackendTask.Env.expect "SESSION_SECRET"
            |> BackendTask.allowFatal
            |> BackendTask.map List.singleton
    , options = Nothing
    }


data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data routeParams request =
    Server.Session.withSession sessionConfig
        (\session ->
            case Server.Session.get "userId" session of
                Just userId ->
                    fetchUserData userId
                        |> BackendTask.map
                            (\userData -> ( session, Response.render userData ))

                Nothing ->
                    BackendTask.succeed
                        ( session, Response.temporaryRedirect "/login" )
        )
        request
```

## Pattern: login action (set session) and logout (clear it)

```elm
-- in the login route's `action`, after verifying credentials:
( session |> Server.Session.insert "userId" user.id
, Response.temporaryRedirect "/dashboard"
)

-- logout:
( session |> Server.Session.remove "userId"
, Response.temporaryRedirect "/login"
)
```

Flash messages (e.g. "Logged in successfully") survive exactly one request:

```elm
( session |> Server.Session.withFlash "message" "Logged in"
, Response.temporaryRedirect "/dashboard"
)
-- next request: Server.Session.get "message" session == Just "Logged in"
```

## Raw cookies: Server.SetCookie

For cookies outside the session abstraction, build a `SetCookie` and attach it with
`Server.Response.withSetCookieHeader`:

```elm
Response.temporaryRedirect "/dashboard"
    |> Response.withSetCookieHeader
        (Server.SetCookie.setCookie "auth" token
            (Server.SetCookie.options
                |> Server.SetCookie.withPath "/"
                |> Server.SetCookie.withMaxAge (60 * 60 * 24 * 7)
                |> Server.SetCookie.withSameSite Server.SetCookie.Lax
            )
        )
```

Defaults from `Server.SetCookie.options` are secure: HttpOnly, Secure. Modifiers:

```elm
withExpiration : Time.Posix -> Options -> Options
withImmediateExpiration : Options -> Options   -- deletes the cookie
withMaxAge : Int -> Options -> Options          -- seconds
withPath / withoutPath / withDomain
withSameSite : SameSite -> Options -> Options   -- Strict | Lax | None
makeVisibleToJavaScript : Options -> Options    -- removes HttpOnly; avoid unless needed
nonSecure : Options -> Options                  -- allows plain HTTP; avoid
```

Read cookies from a request with `Server.Request.cookie : String -> Request -> Maybe String`.

## Gotchas

- Session values are `String`s only — encode richer data yourself (e.g. JSON string).
- Always return the session from the `withSession` handler, even unchanged; returning
  a modified session is how inserts/removes/flash get persisted.
- Cookie-based sessions are HttpOnly by default: invisible to client-side code. Any
  user info the view needs must be put into `Data` by `data`.
- Don't put secrets in `Data` — everything in `Data` is serialized to the client.
- `withSessionResult` is for distinguishing "no cookie yet" from "tampered/invalid
  cookie"; plain `withSession` just gives an empty session in both cases.
