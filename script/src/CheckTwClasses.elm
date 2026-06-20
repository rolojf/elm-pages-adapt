module CheckTwClasses exposing (run)

{-| Build-time lint that checks every "tw …" class against Tailwind's own design-system.

Scans `app/`, `src/` (Elm `class "tw …"` literals) and `content/` HTML/markdown (HTML
`class="tw …"` / `class='tw …'` attributes). Only the `tw ` marker is validated, so
non-Tailwind classes (e.g. org-mode `outline-2`, `org-ul`) are ignored.

Only static, single-line literals are validated — anything assembled with ++ at runtime,
spanning multiple lines, or using `{.class}` markdown attribute lists is ignored.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.File as File
import BackendTask.Glob as Glob
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Regex


{-| Add here any token that appears inside a "tw …" block but is _not_ a Tailwind
utility (for example a hand-written custom class). Seed after the first run if
necessary.
-}
allowlist : List String
allowlist =
    []


{-| Matches a single- or double-quoted value that starts with optional whitespace,
then "tw ", then captures everything up to the closing quote. Double quotes cover Elm
string literals and HTML attributes; single quotes cover HTML attributes in content
(`class='tw …'`). Elm strings are always double-quoted, so single quotes only affect
content files.
-}
twRegex : Regex.Regex
twRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "['\"]\\s*tw\\s+([^'\"]*)['\"]"


{-| Patterns are relative to the project root: `elm-pages run` resolves globs against
the directory it was invoked from, and `lint:tw` runs from the project root.
-}
globSourceFiles : BackendTask FatalError (List String)
globSourceFiles =
    let
        opts =
            { includeDotFiles = False
            , include = Glob.OnlyFiles
            , followSymbolicLinks = True
            , caseSensitiveMatch = True
            , gitignore = False
            , maxDepth = Nothing
            }
    in
    [ "app/**/*.elm"
    , "src/**/*.elm"
    , "content/**/*.html"
    , "content/**/*.md"
    ]
        |> List.map (Glob.fromStringWithOptions opts)
        |> BackendTask.combine
        |> BackendTask.map List.concat


readFiles : List String -> BackendTask FatalError (List { path : String, content : String })
readFiles paths =
    paths
        |> List.map
            (\path ->
                File.rawFile path
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\content -> { path = path, content = content })
            )
        |> BackendTask.combine


{-| token → list of "<file:line"> locations.
-}
type alias TokenLocations =
    Dict String (List String)


extractTwClasses : List { path : String, content : String } -> TokenLocations
extractTwClasses files =
    List.foldl processFile Dict.empty files


processFile : { path : String, content : String } -> TokenLocations -> TokenLocations
processFile file locs =
    let
        indexedLines =
            String.lines file.content
                |> List.indexedMap (\i line -> ( i + 1, line ))
    in
    List.foldl (processLine file.path) locs indexedLines


processLine : String -> ( Int, String ) -> TokenLocations -> TokenLocations
processLine filePath ( lineNum, line ) locs =
    Regex.find twRegex line
        |> List.foldl (addTokens filePath lineNum) locs


addTokens : String -> Int -> Regex.Match -> TokenLocations -> TokenLocations
addTokens filePath lineNum match locs =
    case match.submatches of
        (Just rawClasses) :: _ ->
            rawClasses
                |> String.words
                |> List.filter (String.isEmpty >> not)
                |> List.foldl (addToken filePath lineNum) locs

        _ ->
            locs


addToken : String -> Int -> String -> TokenLocations -> TokenLocations
addToken filePath lineNum token locs =
    Dict.update token
        (\maybeLocs ->
            case maybeLocs of
                Just locList ->
                    Just (locList ++ [ filePath ++ ":" ++ String.fromInt lineNum ])

                Nothing ->
                    Just [ filePath ++ ":" ++ String.fromInt lineNum ]
        )
        locs


run : Script
run =
    globSourceFiles
        |> BackendTask.andThen readFiles
        |> BackendTask.andThen
            (\files ->
                Script.log ("Files: " ++ String.fromInt (List.length files))
                    |> BackendTask.andThen
                        (\() ->
                            let
                                locs =
                                    extractTwClasses files
                            in
                            Script.log ("Tokens: " ++ String.fromInt (Dict.size locs))
                                |> BackendTask.andThen (\() -> validateAndReport locs)
                        )
            )
        |> Script.withoutCliOptions


validateAndReport : TokenLocations -> BackendTask FatalError ()
validateAndReport locs =
    let
        candidates : List String
        candidates =
            Dict.keys locs
                |> List.filter (\token -> not (List.member token allowlist))
    in
    if List.isEmpty candidates then
        Script.log "✓ tw-lint: no classes found to validate."

    else
        validateTwClasses candidates
            |> BackendTask.andThen (report locs candidates)


{-| Calls the Node wired design-system to see which candidates generate no CSS.
-}
validateTwClasses : List String -> BackendTask FatalError (List String)
validateTwClasses candidates =
    BackendTask.Custom.run "validateTwClasses"
        (Encode.object
            [ ( "candidates", Encode.list Encode.string candidates )
            , ( "base", Encode.string "." )
            ]
        )
        (Decode.list Decode.string)
        |> BackendTask.allowFatal


report : TokenLocations -> List String -> List String -> BackendTask FatalError ()
report locs candidates invalid =
    if List.isEmpty invalid then
        Script.log <|
            "✓ tw-lint: "
                ++ String.fromInt (List.length candidates)
                ++ " classes, all valid"

    else
        let
            header =
                Script.log <|
                    "✗ tw-lint: "
                        ++ String.fromInt (List.length invalid)
                        ++ " suspected typo(s):"

            details =
                invalid
                    |> List.sort
                    |> List.map
                        (\token ->
                            let
                                locations =
                                    Dict.get token locs
                                        |> Maybe.withDefault []
                                        |> String.join "\n      "
                            in
                            Script.log <|
                                "  "
                                    ++ token
                                    ++ "\n      "
                                    ++ locations
                        )
        in
        header
            |> BackendTask.andThen (\() -> BackendTask.sequence details)
            |> BackendTask.map (always ())
            |> BackendTask.andThen
                (\() ->
                    BackendTask.fail <|
                        FatalError.fromString "tw-lint failed: found invalid Tailwind classes"
                )
