module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import HardCodedData
import Html exposing (Html)
import Json.Encode as Encode
import LanguageTag exposing (LanguageTag, emptySubtags)
import LanguageTag.Language
import LanguageTag.Region as Region
import MimeType
import Pages.Manifest as Manifest
import Pages.Manifest.Category as Category
import Pages.Url as Url
import Route exposing (Route)
import Server.Request as Request
import Server.Response as Response
import Server.SetCookie as SetCookie
import Shared
import Site
import Time
import Visitas


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response) -- antes htmlToString era (Html Never -> String)
routes getStaticRoutes htmlToString =
    [ BackendTask.succeed manifest
        |> Manifest.generator Site.canonicalUrl
    , apiV2
    ]


apiV2 : ApiRoute ApiRoute.Response
apiV2 =
    ApiRoute.succeed
        (\eventDotJson request ->
            let
                event =
                    String.replace ".json" "" eventDotJson

                cookieId =
                    Request.cookie "visitor_id" request
            in
            Visitas.registra
                { site = HardCodedData.siteName
                , token = Nothing
                , visitorId = Maybe.withDefault "" cookieId
                , event = event
                , ts = Time.posixToMillis (Request.requestTime request)
                , ua = Request.header "user-agent" request
                , referrer = Request.header "referer" request
                , ip = Request.header "x-forwarded-for" request
                }
                |> BackendTask.map
                    (\res ->
                        let
                            base =
                                Response.json
                                    (Encode.object
                                        [ ( "ok", Encode.bool True )
                                        , ( "count", Encode.int res.count )
                                        ]
                                    )
                        in
                        case cookieId of
                            Just _ ->
                                base

                            Nothing ->
                                base
                                    |> Response.withSetCookieHeader
                                        (SetCookie.setCookie "visitor_id" res.visitorId
                                            (SetCookie.options
                                                |> SetCookie.withPath "/"
                                                |> SetCookie.withMaxAge (60 * 60 * 24 * 365)
                                                |> SetCookie.withSameSite SetCookie.Lax
                                            )
                                        )
                    )
        )
        |> ApiRoute.literal "api-v2"
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.serverRender


manifest : Manifest.Config
manifest =
    let
        iconos =
            [ { src = Url.external "/icon-192.png"
              , sizes = [ ( 192, 192 ) ]
              , mimeType = Just MimeType.Png
              , purposes = [ Manifest.IconPurposeAny ]
              }
            , { src = Url.external "/icon-512.png"
              , sizes = [ ( 512, 512 ) ]
              , mimeType = Just MimeType.Png
              , purposes = [ Manifest.IconPurposeAny ]
              }
            ]
    in
    Manifest.init
        { name = HardCodedData.siteName
        , description = ""
        , startUrl = Route.Index |> Route.toPath
        , icons = iconos
        }
        |> Manifest.withCategories [ Category.business ]
        |> Manifest.withLang
            (LanguageTag.Language.es
                |> LanguageTag.build
                    { emptySubtags
                        | region = Just Region.mx
                    }
            )
