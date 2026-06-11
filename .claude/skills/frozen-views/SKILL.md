---
name: frozen-views
description: EXPERIMENTAL elm-pages bundle-size optimization - View.freeze renders parts of a view at build/server time so the client adopts the HTML without shipping the rendering code (dead-code elimination of markdown parsers, syntax highlighters, and unused Data fields). Use only when optimizing client bundle size, and only after verifying the installed elm-pages version supports it.
---

# elm-pages: Frozen Views (EXPERIMENTAL)

**Verify first**: this feature is not in the stable API reference. Check that the
installed elm-pages version exposes `View.freeze` support (e.g. the starter's
`View.elm` mentions `freezableToHtml`, or the feature is listed in the version's
release notes) before using it.

## What it does

`View.freeze` marks a section of a Route Module's `view` as server-only. It is
rendered to HTML at build time (pre-rendered routes) or request time
(server-rendered routes); the client **adopts** that HTML without re-rendering.
Consequences:

- The rendering code and its dependencies (markdown parsers, syntax highlighters,
  formatters) are dead-code eliminated from the client JS bundle.
- `Data` fields used *only* inside frozen regions (or only in `head`) are stripped
  from `content.dat` — never sent to the client.
- Works with form actions: after a submit, the server re-renders frozen HTML with
  the new `app.action` and the client adopts it.

## Usage

```elm
view : App Data ActionData RouteParams -> Model -> View (PagesMsg Msg)
view app model =
    { title = app.data.title
    , body =
        [ -- FROZEN: rendered server-side, code eliminated from client bundle
          View.freeze
            (div []
                [ h1 [] [ text app.data.title ]
                , app.data.rawMarkdown |> markdownRenderView
                ]
            )

        -- DYNAMIC: uses model, stays in the client bundle
        , button [ onClick (PagesMsg.fromMsg Increment) ]
            [ text (String.fromInt model.counter) ]
        ]
    }
```

## Constraints

- Frozen content must be `Html Never` — no event handlers (compile error otherwise).
  Forms can't be frozen; a frozen view *showing* `app.action` results can be.
- Frozen content may use `app.data` and `app.action`, **not `model`** (including
  values derived from `model` via let bindings or case expressions). Referencing
  `model` de-optimizes that freeze call: it still works, but no code is eliminated.
- `View.freeze` only works inside Route Module `view` functions — not in
  `Shared.elm` or helper modules (current limitation).
- `elm-pages build --strict` fails the build on any de-optimized freeze call
  (useful in CI). Without it, de-optimization is just a warning.
- Inspect what got eliminated with `elmjs-inspect dist/elm.js.opt`.

## Setup: View.elm must export freeze helpers

```elm
module View exposing (View, map, freeze, freezableToHtml, htmlToFreezable)

import Html exposing (Html)


type alias View msg =
    { title : String, body : List (Html msg) }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title, body = List.map (Html.map fn) doc.body }


type alias Freezable =
    Html Never


freezableToHtml : Freezable -> Html Never
freezableToHtml =
    identity


htmlToFreezable : Html Never -> Freezable
htmlToFreezable =
    identity


freeze : Freezable -> Html msg
freeze content =
    content |> freezableToHtml |> htmlToFreezable |> Html.map never
```

With `Html.Styled` (elm-css): `Freezable = Html.Styled.Html Never`,
`freezableToHtml = Html.Styled.toUnstyled`, `htmlToFreezable = Html.Styled.fromUnstyled`,
and `freeze` ends with `Html.Styled.map never`.

## When to freeze

Good candidates: content with no interactivity, rendered from `app.data`/`app.action`,
with heavy render dependencies (markdown, syntax highlighting). Reference result:
the elm-pages.com docs site cut its bundle ~52% raw / ~47% gzipped.
