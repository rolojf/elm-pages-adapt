---
name: seo-api-routes
description: Head tags, SEO, and non-HTML endpoints in elm-pages - Head/Head.Seo (meta, OpenGraph, Twitter cards, JSON-LD), Site.elm global config, Pages.Manifest (PWA), and ApiRoute in app/Api.elm for RSS feeds, sitemaps, robots.txt, and JSON APIs. Use when adding meta tags, social previews, feeds, or API endpoints.
---

# elm-pages: SEO, head tags, API routes

## Per-route head tags

Every Route Module has `head : App Data ActionData RouteParams -> List Head.Tag`,
resolved at build/request time with full access to `app.data`.

Prefer the high-level `Head.Seo` helpers (they emit correct OpenGraph + Twitter
tags together) over manual `Head.metaProperty` calls:

```elm
import Head
import Head.Seo as Seo
import Pages.Url


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    Seo.summaryLarge
        { canonicalUrlOverride = Nothing
        , siteName = "My Blog"
        , image =
            { url = Pages.Url.external "https://example.com/og.png"
            , alt = app.data.title
            , dimensions = Just { width = 1200, height = 630 }
            , mimeType = Nothing
            }
        , description = app.data.description
        , title = app.data.title
        , locale = Nothing
        }
        |> Seo.website
```

Card constructors (return `Common`): `summary`, `summaryLarge`, `audioPlayer`,
`videoPlayer`. OpenGraph type functions (take `Common`, return `List Head.Tag`):
`website`, `article { tags, section, publishedTime, modifiedTime, expirationTime }`,
`book`, `profile`, `song`.

Low-level `Head` module when needed:

```elm
Head.metaName "robots" (Head.raw "noindex")
Head.canonicalLink (Just url)
Head.rssLink "/blog/feed.xml"
Head.sitemapLink "/sitemap.xml"
Head.icon [ ( 32, 32 ) ] MimeType.Png iconUrl
Head.appleTouchIcon (Just 180) iconUrl
Head.rootLanguage langTag
Head.structuredData jsonLdValue        -- JSON-LD
Head.nonLoadingNode tag attrs          -- escape hatch
```

URLs: `Pages.Url.external "https://..."` or `Pages.Url.fromPath`.

## Site.elm — global head tags

`app/Site.elm` exposes `config : SiteConfig` with the canonical site URL and head
tags included on every page (e.g. `Head.sitemapLink`, favicons, manifest link).
Static `<head>` HTML (stylesheets, analytics) can also come from
`headTagsTemplate` in `elm-pages.config.mjs`.

## API routes — `app/Api.elm`

`app/Api.elm` exposes `routes`, a list of `ApiRoute`s. Each is a path builder plus a
rendering strategy:

```elm
-- path builder
ApiRoute.succeed handler
    |> ApiRoute.literal "api"
    |> ApiRoute.slash
    |> ApiRoute.capture            -- adds a String arg to the handler
-- strategy
    |> ApiRoute.single                         -- one file at build time
    |> ApiRoute.preRender (\ctor -> taskOfParamLists)  -- many files at build time
    |> ApiRoute.serverRender                   -- dynamic, gets a Request
```

### Pre-rendered file (RSS feed, sitemap, robots.txt)

```elm
rss : ApiRoute ApiRoute.Response
rss =
    ApiRoute.succeed
        (blogPosts |> BackendTask.map renderRssXml)   -- BackendTask FatalError String
        |> ApiRoute.literal "blog/feed.xml"
        |> ApiRoute.single
```

### Server-rendered JSON endpoint

```elm
userApi : ApiRoute ApiRoute.Response
userApi =
    ApiRoute.succeed
        (\userId request ->
            getUserById userId
                |> BackendTask.map (\user -> Server.Response.json (encodeUser user))
        )
        |> ApiRoute.literal "api"
        |> ApiRoute.slash
        |> ApiRoute.literal "users"
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.serverRender
```

`serverRender` handlers receive a `Server.Request.Request` and return
`BackendTask FatalError (Server.Response.Response Never Never)` — build bodies with
`Server.Response.json/plainText/body` and adjust with `withStatusCode`/`withHeader`.
`preRenderWithFallback` pre-builds known paths and serves the rest at request time.

## PWA manifest — `Pages.Manifest`

```elm
manifest : Pages.Manifest.Config
manifest =
    Pages.Manifest.init
        { name = "My App"
        , description = "..."
        , startUrl = UrlPath.fromString "/"
        , icons = [ { src = iconUrl, sizes = [ ( 512, 512 ) ], mimeType = Nothing, purposes = [] } ]
        }
        |> Pages.Manifest.withThemeColor color
        |> Pages.Manifest.withDisplayMode Pages.Manifest.Standalone
```

Serve it as an ApiRoute with `Pages.Manifest.generator canonicalUrl (BackendTask.succeed manifest)`
and reference it via `Head.manifestLink` in `Site.elm`.

## Gotchas

- `head` only runs server-side; data used only in `head` never reaches the client.
- ApiRoute paths are literal strings — no kebab-case conversion like Route Modules.
- Server-rendered ApiRoutes need a deployment adapter (see `deployment` skill);
  `single`/`preRender` ones are just files in `dist/`.
