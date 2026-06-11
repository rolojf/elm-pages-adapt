---
name: forms
description: Forms and user input in elm-pages - defining forms with dillonkearns/elm-form, rendering with Pages.Form.renderHtml, handling submissions in the route's `action` with Server.Request.formData, validation, ConcurrentSubmission, and Pages.Fetcher. Use when pages accept user input (login forms, CRUD, search). Requires server-rendered routes for the `action` to run.
---

# elm-pages: Forms

elm-pages integrates `dillonkearns/elm-form`. Forms submit to the route's `action`
(server-side BackendTask) and work without JavaScript (progressive enhancement);
when hydrated, elm-pages intercepts the submit and manages in-flight state for you.
Form state lives in the framework (`app.pageFormState`), not in your `Model`.

## Defining a form

```elm
import Form
import Form.Field as Field
import Form.FieldView as FieldView
import Form.Validation as Validation


type alias SignUpForm =
    { username : String, password : String }


signUpForm : Form.HtmlForm String SignUpForm input msg
signUpForm =
    (\username password ->
        { combine =
            Validation.succeed SignUpForm
                |> Validation.andMap username
                |> Validation.andMap password
        , view =
            \formState ->
                [ FieldView.input [] username
                , FieldView.input [] password
                , Html.button [] [ Html.text "Sign up" ]
                ]
        }
    )
        |> Form.form
        |> Form.field "username" (Field.text |> Field.required "Required")
        |> Form.field "password" (Field.password |> Field.required "Required")
```

`combine` parses fields into your type; `view` renders them. Field kinds include
`Field.text`, `Field.password`, `Field.checkbox`, `Field.int`, `Field.float`,
`Field.date`, `Field.time`, with refinements like `Field.required`, `Field.withMin`.

## Rendering in `view`

Use `Pages.Form.renderHtml` (NOT raw `Form.renderHtml`) so the form integrates with
elm-pages navigation/submission state:

```elm
import Pages.Form


view app shared =
    { title = "Sign up"
    , body =
        [ signUpForm
            |> Pages.Form.renderHtml []
                (Form.options "signup")
                app
        ]
    }
```

There is also `Pages.Form.renderStyledHtml` for `Html.Styled` (elm-css). For
fire-and-forget submissions that don't block navigation, wrap the options with
`Pages.Form.withConcurrent` and track progress via `app.concurrentSubmissions :
Dict String (ConcurrentSubmission (Maybe action))` (status: `Submitting | Reloading
actionData | Complete actionData`).

## Handling submission in `action`

```elm
import Form.Handler
import Server.Request exposing (Request)
import Server.Response as Response


type alias ActionData =
    { errors : List String }


formHandlers : Form.Handler.Handler String SignUpForm
formHandlers =
    Form.Handler.init identity signUpForm
    -- multiple forms on one page: |> Form.Handler.with TagB otherForm


action : RouteParams -> Request -> BackendTask FatalError (Response ActionData ErrorPage)
action routeParams request =
    case Server.Request.formData formHandlers request of
        Just ( serverResponse, Form.Valid parsed ) ->
            createUser parsed
                |> BackendTask.map
                    (\_ -> Response.temporaryRedirect "/welcome")

        Just ( serverResponse, Form.Invalid _ _ ) ->
            BackendTask.succeed (Response.render { errors = [ "Invalid form" ] })

        Nothing ->
            BackendTask.succeed (Response.render { errors = [] })
```

`Server.Request.formData : Form.Handler.Handler error combined -> Request ->
Maybe ( Form.ServerResponse error, Form.Validated error combined )`. The `Maybe` is
`Nothing` when the request has no form payload. Validation runs on both client (as
the user types) and server (authoritative) from the same form definition.

For validations that need a BackendTask (e.g. "username taken"), use
`Server.Request.formDataWithServerValidation` with a `Pages.Form.Handler`.

After a successful action that renders (rather than redirects), the page's `data`
reloads and `app.action : Maybe ActionData` carries the action's payload to `view` —
use it to show success/error feedback.

## Multiple forms on one page

Give each form a distinct id in `Form.options`, and combine them into one handler
with variants:

```elm
type Action
    = SignUp SignUpForm
    | DeleteAccount


formHandlers : Form.Handler.Handler String Action
formHandlers =
    Form.Handler.init SignUp signUpForm
        |> Form.Handler.with (\() -> DeleteAccount) deleteForm
```

Hidden discriminator fields via `Form.hiddenField` / `Form.hiddenKind` help route
submissions to the right variant.

## Programmatic submission: Pages.Fetcher

```elm
Pages.Fetcher.submit decoder
    { fields = [ ( "name", "value" ) ], headers = [] }
    -- : Fetcher (Result Http.Error decoded)
```

Use for submissions triggered from `update` instead of a DOM form.

## Gotchas

- The `action` runs server-side only; it has full `Request` access and can use
  sessions/cookies (see `sessions-auth`).
- Prefer redirect-after-post (`Response.temporaryRedirect`) on success to avoid
  resubmission on refresh.
- Form `view` functions receive `formState` with `errors`, `submitting`, etc. — use
  it for inline error display and disabling submit buttons.
- `app.navigation : Maybe Navigation` exposes `Submitting`/`LoadAfterSubmit` states
  for global loading indicators.
