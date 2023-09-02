module MySession exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import Codec
import FatalError exposing (FatalError)
import Route
import Server.Request exposing (Request)
import Server.Response exposing (Response)
import Server.Session as Session exposing (Session)
import Server.SetCookie as SetCookie


cookieOptions : Maybe SetCookie.Options
cookieOptions =
    SetCookie.options
        |> SetCookie.withPath "/"
        |> SetCookie.withSameSite SetCookie.Strict
        |> Just


secrets : BackendTask error (List String)
secrets =
    "saltSalt"
        |> List.singleton
        |> BackendTask.succeed


registro =
    { name = "mysession"
    , secrets = secrets
    , options = cookieOptions
    }


withSession :
    (Result Session.NotLoadedReason Session.Session -> BackendTask FatalError ( Session.Session, Response data errorPage ))
        -> Request
        -> BackendTask FatalError (Response data errorPage)
withSession =
    Session.withSessionResult
        registro

withSessionOrRedirect :
    (Session.Session -> BackendTask FatalError ( Session.Session, Response data errorPage ))
    -> Request
    -> BackendTask FatalError (Response data errorPage)
withSessionOrRedirect toRequest handler =
    Session.withSessionResult
        registro
        (\sessionResult ->
            sessionResult
                |> Result.map toRequest
                |> Result.withDefault
                    (BackendTask.succeed
                        ( Session.empty
                        , Route.redirectTo Route.Login
                        )
                    )
        )
        handler

expectSessionOrRedirect :
    (Session.Session -> BackendTask FatalError ( Session.Session, Response data errorPage ))
    -> Request
    -> BackendTask FatalError (Response data errorPage)
expectSessionOrRedirect toRequest request =
    Session.withSessionResult
        { name = "mysession"
        , secrets = secrets
        , options = Nothing
        }
        (\sessionResult ->
            sessionResult
                |> Result.map toRequest
                |> Result.withDefault
                    (BackendTask.succeed
                        ( Session.empty
                        , Route.redirectTo Route.Login
                        )
                    )
        )
        request


schema =
    { name = ( "name", Codec.string )
    , message = ( "message", Codec.string )
    , user =
        ( "user"
        , Codec.object User
            |> Codec.field "id" .id Codec.int
            |> Codec.buildObject
        )
    }


type alias User =
    { id : Int
    }
