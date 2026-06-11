## Project: elm-pages-adapt

This is **public template** I use for different web pages using an elm-pages 3.x framework (pre-rendered + server-rendered hybrid) to be deployed on Netlify previously on AWS. The site uses Tailwind CSS for styling and Cloudinary for image delivery.  The following websites uses this template as a base and modifies it to customize the web page:

- https://github.com/rolojf/psm1
- https://github.com/rolojf/cfd3
- https://github.com/rolojf/solarpaq1
- https://github.com/rolojf/psolar
- https://github.com/rolojf/reftex1

Some of this webpages might be out of sync of recent changes with this repo.


### Custom `View` Type

`View.elm` extends the standard elm-pages view with a required `withMenu` field:

```elm
type alias View msg =
    { title : String
    , body : List (Html msg)
    , withMenu : MenuInfo  -- NoMenu | SiMenu (List Liga)
    }
```

Every route must set `withMenu`. Use `NoMenu` for pages without navigation, `SiMenu links` to populate the nav.

### Custom `Effect` Type

- `Effect.elm` defines project-specific effects beyond standard `Cmd`:
- `EsperaPues Float msg` — delayed message (like setTimeout)
- `SoloAccedeLiga String (Result Http.Error String -> msg)` — fire a GET request
- `PushUrl String` — programmatic navigation
- `Enfoca msg String` — focus a DOM element

### Site Configuration (`src/HardCodedData.elm`)

All hardcoded site constants live here: `canonicalUrl`, `siteName` (content folder name), image config (`logoTrans`, `logoResource`), and locale. **Change this file when setting up a new site instance.**

### Images via Cloudinary

Use `MiCloudinary.url transforms asset` to build Cloudinary URLs:
```elm
MiCloudinary.url "f_auto,q_auto,w_800" "v123/my-image.jpg"
```


### Styling

Two CSS files are in play:
- `input.css` → compiled to `tailwind.css` (Tailwind v3 with plugins: forms, typography, aspect-ratio, line-clamp)
- `style.css` — global styles, loaded directly in the HTML head

Tailwind content scanning covers `app/**/*.elm` and `src/**/*.elm`. The custom extractor parses Elm string literals of the form `"tw <classes>"`.


### Analytics

`Analytics.elm` provides `Event` type and `toEffect`. Use `Analytics.eventoXReportar "event-name"` to create events, then convert with `Analytics.toEffect`.

### Shared State

`Shared.elm` tracks `UsuarioSt` (auth state: `Desconocido | Rechazado | Conocido`), menu toggle state, and fires analytics. The `template.onPageChange` is set to `Just OnPageChange` (unlike the starter which uses `Nothing`).


### Deployment

Deployed to Netlify (adapter: `elm-pages/adapter/netlify.js`). Netlify Functions go in `functions/`, built output in `dist/`. Also supports S3 deployment via `npm run asdf`.
