module Route.Login exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Css exposing (true)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Form
import Form.Field as Field
import Form.FieldView
import Form.Handler
import Form.Validation as Validation exposing (Validation)
import Head
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import MySession
import Pages.Form
import PagesMsg exposing (PagesMsg)
import Route
import RouteBuilder exposing (App, StatefulRoute, StatelessRoute)
import Server.Request as Request exposing (Request)
import Server.Response as Response exposing (Response)
import Server.Session as Session
import Shared
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


type alias ActionData =
    { errors : Form.ServerResponse String
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = action
        }
        |> RouteBuilder.buildNoState { view = view }


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action routeParams request =
    request
        |> MySession.withSession
            (\session ->
                case request |> Request.formDataWithServerValidation (form |> Form.Handler.init identity) of
                    Nothing ->
                        BackendTask.fail (FatalError.fromString "Invalid form response")

                    Just nameResultData ->
                        nameResultData
                        |> BackendTask.map
                        (\nameResult ->
                            case nameResult of
                                Err errors ->
                                    ( session
                                        |> Result.withDefault Session.empty
                                    , Response.render
                                        { errors = errors
                                        }
                                    )

                                Ok ( _, name ) ->
                                    ( session
                                        |> Result.withDefault Session.empty
                                        |> Session.insert "name" name
                                        |> Session.withFlash "message" ("Welcome " ++ name ++ "!")
                                    , Route.redirectTo Route.Index
                                    )
                        )
            )


type alias Data =
    { username : Maybe String
    , flashMessage : Maybe String
    }


form : Pages.Form.FormWithServerValidations String String input (List (Html msg))
form =
    Form.form
        (\username ->
            { combine =
                Validation.succeed identity
                    |> Validation.andMap username
                    |> Validation.map
                        (\clientValidated ->
                            BackendTask.succeed
                                (Validation.succeed clientValidated
                                    |> Validation.withErrorIf
                                        (clientValidated == "error")
                                        username
                                        "Invalid username"
                                )
                        )
            , view =
                \formState ->
                    let
                        errors : Validation.Field String parsed anyKind -> List String
                        errors field =
                            formState.errors
                                |> Form.errorsForField field

                        errorsView : Validation.Field String parsed anyKind -> Html msg
                        errorsView field =
                            case
                                ( formState.submitAttempted
                                , errors field
                                )
                            of
                                ( _, first :: rest ) ->
                                    Html.div []
                                        [ Html.ul
                                            [ class "tw mt-1" ]
                                            (List.map
                                                (\error ->
                                                    Html.li [ class "tw mt-2 text-sm text-red-600" ]
                                                        [ Html.text error ]
                                                )
                                                (first :: rest)
                                            )
                                        ]

                                _ ->
                                    Html.div [] []

                        claseAro : Validation.Field String parsed anyKind -> String
                        claseAro field =
                            case
                                ( formState.submitAttempted
                                , errors field
                                )
                            of
                                ( True, first :: rest ) ->
                                    "tw block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-red-600 sm:text-sm sm:leading-6"

                                _ ->
                                    "tw block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"

                        fieldView : String -> Validation.Field String parsed Form.FieldView.Input -> Html msg
                        fieldView label field =
                            Html.div []
                                [ Html.label [ class "tw block text-sm font-medium leading-6 text-gray-900" ]
                                    [ Html.text (label ++ " ")
                                    , div [ class "tw mt-2" ]
                                        [ field
                                            |> Form.FieldView.input
                                                [ class (claseAro field) ]
                                        ]
                                    ]
                                , errorsView field
                                ]
                    in
                    [ fieldView "Username" username
                    , Html.button
                        [ class "tw flex w-full justify-center rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600" ]
                        [ (if formState.submitting then
                            "Logging in..."

                           else
                            "Log in"
                          )
                            |> Html.text
                        ]
                    ]
            }
        )
        |> Form.field "name" (Field.text |> Field.required "Required")

data : RouteParams -> Request -> BackendTask FatalError (Response Data ErrorPage)
data routeParams request =
    request
        |> MySession.withSession
            (\session ->
                case session of
                    Ok okSession ->
                        let
                            flashMessage : Maybe String
                            flashMessage =
                                okSession
                                    |> Session.get "message"
                        in
                        ( okSession
                        , Data
                            (okSession |> Session.get "name")
                            flashMessage
                            |> Response.render
                        )
                            |> BackendTask.succeed

                    _ ->
                        ( Session.empty
                        , { username = Nothing, flashMessage = Nothing }
                            |> Response.render
                        )
                            |> BackendTask.succeed
            )


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view : App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg Msg)
view app shared =
    { title = "Login"
    , body =
        [ app.data.flashMessage
            |> Maybe.map (\message -> flashView (Ok message))
            |> Maybe.withDefault (Html.p [] [ Html.text "No flash" ])
        , Html.p []
            [ Html.text
                (case app.data.username of
                    Just username ->
                        "Hello " ++ username ++ "!"

                    Nothing ->
                        "You aren't logged in yet."
                )
            ]
        , htmlBody app
        ]
    , withMenu = View.NoMenu
    }


flashView : Result String String -> Html msg
flashView message =
    Html.p
        [ Attr.style "background-color" "rgb(163 251 163)"
        ]
        [ Html.text <|
            case message of
                Ok okMessage ->
                    okMessage

                Err error ->
                    "Something went wrong: " ++ error
        ]


tituloSuperior =
    div
        [ class "tw sm:mx-auto sm:w-full sm:max-w-md"
        ]
        [ {- Html.img
                 [ class "tw mx-auto h-12 w-auto"
                 , Attr.src "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600"
                 , Attr.alt "Your Company"
                 ]
                 []
             ,
          -}
          Html.h2
            [ class "tw mt-6 text-center text-3xl font-bold tracking-tight text-gray-900"
            ]
            [ text "Registra tus datos para acceder" ]

        {- , Html.p
           [ class "tw mt-2 text-center text-sm text-gray-600"
           ]
           [ text "Or"
           , Html.a
               [ Attr.href "#"
               , class "tw font-medium text-indigo-600 hover:text-indigo-500"
               ]
               [ text "start your 14-day free trial" ]
           ]
        -}
        ]


elmInnerForm fullApp =
    form
        |> Pages.Form.renderHtml
            [ class "space-y-6" ]
            (Form.options "form"
                |> Form.withServerResponse (fullApp.action |> Maybe.map .errors)
            )
            fullApp


htmlBody laApp =
    div
        [ class "tw flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8" ]
        [ tituloSuperior
        , div
            [ class "tw mt-8 sm:mx-auto sm:w-full sm:max-w-md" ]
            [ div
                [ class "tw bg-white px-4 py-8 shadow sm:rounded-lg sm:px-10" ]
                [ elmInnerForm laApp ]
            ]
        ]



-- ** Funciones no usadas


innerForm superAccion =
    Html.form
        [ class "space-y-6"
        , Attr.action "#"
        , Attr.method "POST"
        ]
        [ div []
            [ Html.label
                [ Attr.for "email"
                , class "tw block text-sm font-medium leading-6 text-gray-900"
                ]
                [ text "Email address" ]
            , div
                [ class "tw mt-2"
                ]
                [ Html.input
                    [ Attr.id "email"
                    , Attr.name "email"
                    , Attr.type_ "email"
                    , Attr.attribute "autocomplete" "email"
                    , Attr.required True
                    , class "tw block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"
                    ]
                    []
                ]
            ]
        , div []
            [ Html.label
                [ Attr.for "password"
                , class "tw block text-sm font-medium leading-6 text-gray-900"
                ]
                [ text "Password" ]
            , div
                [ class "tw mt-2"
                ]
                [ Html.input
                    [ Attr.id "password"
                    , Attr.name "password"
                    , Attr.type_ "password"
                    , Attr.attribute "autocomplete" "current-password"
                    , Attr.required True
                    , class "tw block w-full rounded-md border-0 py-1.5 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm sm:leading-6"
                    ]
                    []
                ]
            ]
        , div []
            [ Html.button
                [ Attr.type_ "submit"
                , class "tw flex w-full justify-center rounded-md bg-indigo-600 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
                ]
                [ text "Sign in" ]
            ]
        ]



{- RecomendacionesDelTemplate
      This example requires some changes to your config:

      ```
      // tailwind.config.js
      module.exports = {
        // ...
        plugins: [
          // ...
          require('@tailwindcss/forms'),
        ],
      }
      ```
   This example requires updating your template:

      ```
      <html class="h-full bg-gray-50">
      <body class="h-full">
      ```
-}


opcionParaContinuar =
    div
        [ class "mt-6" ]
        [ div
            [ class "relative" ]
            [ div [ class "tw absolute inset-0 flex items-center" ]
                [ div [ class "tw w-full border-t border-gray-300" ] []
                ]
            , div
                [ class "tw relative flex justify-center text-sm" ]
                [ Html.span
                    [ class "tw bg-white px-2 text-gray-500" ]
                    [ text "Or continue with" ]
                ]
            ]
        ]


signInConRedesSociales =
    div
        [ class "tw mt-6 grid grid-cols-3 gap-3" ]
        [ div []
            [ Html.a
                [ Attr.href "#"
                , class "tw inline-flex w-full justify-center rounded-md bg-white px-4 py-2 text-gray-500 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 focus:outline-offset-0"
                ]
                [ Html.span
                    [ class "tw sr-only" ]
                    [ text "Sign in with Facebook" ]
                , svg
                    [ SvgAttr.class "tw h-5 w-5"
                    , SvgAttr.fill "currentColor"
                    , SvgAttr.viewBox "0 0 20 20"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ path
                        [ SvgAttr.fillRule "evenodd"
                        , SvgAttr.d "M20 10c0-5.523-4.477-10-10-10S0 4.477 0 10c0 4.991 3.657 9.128 8.438 9.878v-6.987h-2.54V10h2.54V7.797c0-2.506 1.492-3.89 3.777-3.89 1.094 0 2.238.195 2.238.195v2.46h-1.26c-1.243 0-1.63.771-1.63 1.562V10h2.773l-.443 2.89h-2.33v6.988C16.343 19.128 20 14.991 20 10z"
                        , SvgAttr.clipRule "evenodd"
                        ]
                        []
                    ]
                ]
            ]
        , div []
            [ Html.a
                [ Attr.href "#"
                , class "tw inline-flex w-full justify-center rounded-md bg-white px-4 py-2 text-gray-500 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 focus:outline-offset-0"
                ]
                [ Html.span
                    [ class "tw sr-only" ]
                    [ text "Sign in with Twitter" ]
                , svg
                    [ SvgAttr.class "tw h-5 w-5"
                    , SvgAttr.fill "currentColor"
                    , SvgAttr.viewBox "0 0 20 20"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ path
                        [ SvgAttr.d "M6.29 18.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0020 3.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.073 4.073 0 01.8 7.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 010 16.407a11.616 11.616 0 006.29 1.84" ]
                        []
                    ]
                ]
            ]
        , div []
            [ Html.a
                [ Attr.href "#"
                , class "tw inline-flex w-full justify-center rounded-md bg-white px-4 py-2 text-gray-500 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 focus:outline-offset-0"
                ]
                [ Html.span
                    [ class "sr-only" ]
                    [ text "Sign in with GitHub" ]
                , svg
                    [ SvgAttr.class "tw h-5 w-5"
                    , SvgAttr.fill "currentColor"
                    , SvgAttr.viewBox "0 0 20 20"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ path
                        [ SvgAttr.fillRule "evenodd"
                        , SvgAttr.d "M10 0C4.477 0 0 4.484 0 10.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0110 4.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.203 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.942.359.31.678.921.678 1.856 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0020 10.017C20 4.484 15.522 0 10 0z"
                        , SvgAttr.clipRule "evenodd"
                        ]
                        []
                    ]
                ]
            ]
        ]


rembembers =
    div
        [ class "tw flex items-center justify-between"
        ]
        [ div
            [ class "tw flex items-center"
            ]
            [ Html.input
                [ Attr.id "remember-me"
                , Attr.name "remember-me"
                , Attr.type_ "checkbox"
                , class "tw h-4 w-4 rounded border-gray-300 text-indigo-600 focus:ring-indigo-600"
                ]
                []
            , Html.label
                [ Attr.for "remember-me"
                , class "tw ml-2 block text-sm text-gray-900"
                ]
                [ text "Remember me" ]
            ]
        , div
            [ class "tw text-sm"
            ]
            [ Html.a
                [ Attr.href "#"
                , class "tw font-medium text-indigo-600 hover:text-indigo-500"
                ]
                [ text "Forgot your password?" ]
            ]
        ]
