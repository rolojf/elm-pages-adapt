module Visitas exposing (Geo, RegistroVisita, Resultado, registra)

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias RegistroVisita =
    { site : String
    , token : Maybe String
    , visitorId : String -- "" => Node mints one
    , event : String
    , ts : Int
    , ua : Maybe String
    , referrer : Maybe String
    , ip : Maybe String -- from x-forwarded-for
    }


type alias Resultado =
    { visitorId : String, count : Int }


type alias Geo =
    { country : Maybe String, region : Maybe String, city : Maybe String }


registra : RegistroVisita -> BackendTask FatalError Resultado
registra r =
    resuelveGeo r.ip
        |> BackendTask.andThen
            (\geo ->
                BackendTask.Custom.run "logVisit"
                    (Encode.object
                        [ ( "site", Encode.string r.site )
                        , ( "token", maybeStr r.token )
                        , ( "visitorId", Encode.string r.visitorId )
                        , ( "event", Encode.string r.event )
                        , ( "ts", Encode.int r.ts )
                        , ( "ua", maybeStr r.ua )
                        , ( "referrer", maybeStr r.referrer )
                        , ( "country", maybeStr geo.country )
                        , ( "region", maybeStr geo.region )
                        , ( "city", maybeStr geo.city )
                        ]
                    )
                    (Decode.map2 Resultado
                        (Decode.field "visitorId" Decode.string)
                        (Decode.field "count" Decode.int)
                    )
                    |> BackendTask.allowFatal
            )


{-| best-effort: IP -> coarse geo via ipapi.co; never fails the visit log
-}
resuelveGeo : Maybe String -> BackendTask FatalError Geo
resuelveGeo maybeIp =
    case maybeIp |> Maybe.andThen primeraIp of
        Nothing ->
            BackendTask.succeed geoVacia

        Just ip ->
            BackendTask.Http.getJson ("https://ipapi.co/" ++ ip ++ "/json/") geoDecoder
                |> BackendTask.onError (\_ -> BackendTask.succeed geoVacia)


geoVacia : Geo
geoVacia =
    { country = Nothing, region = Nothing, city = Nothing }


geoDecoder : Decoder Geo
geoDecoder =
    Decode.map3 Geo
        (Decode.maybe (Decode.field "country_name" Decode.string))
        (Decode.maybe (Decode.field "region" Decode.string))
        (Decode.maybe (Decode.field "city" Decode.string))


primeraIp : String -> Maybe String
primeraIp s =
    -- x-forwarded-for may be "ip1, ip2, ip3"
    s
        |> String.split ","
        |> List.head
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\x ->
                if String.isEmpty x then
                    Nothing

                else
                    Just x
            )


maybeStr : Maybe String -> Encode.Value
maybeStr m =
    m |> Maybe.map Encode.string |> Maybe.withDefault Encode.null
