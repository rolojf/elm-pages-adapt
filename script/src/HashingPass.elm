module HashingPass exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Base64.Encode as B64e
import Bytes exposing (Bytes)
import Bytes.Encode
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm
import Elm.Let as Let exposing (Let)
import Elm.ToString
import FatalError exposing (FatalError)
import HashingFunc exposing (..)
import HmacSha1
import HmacSha1.Key as Key
import Json.Decode
import PBKDF2 exposing (pbkdf2)
import Pages.Script as Script exposing (Script)


run : Script
run =
    File.onlyFrontmatter
        allAccessDecoder
        "../acceso.yaml"
        |> BackendTask.allowFatal
        |> BackendTask.map
            (List.map (accessDataToString >> computeHash))
        |> BackendTask.map (generandoCodigo >> .contents)
        |> BackendTask.mapError (\_ -> hazloRecuperable)
        |> BackendTask.andThen escribeUnArchivo
        |> BackendTask.allowFatal
        |> Script.withoutCliOptions


hazloRecuperable =
    Script.FileWriteError
        |> FatalError.recoverable
            { title = "Falló al escribir el archivo"
            , body = "Búscale para más detalle"
            }


escribeUnArchivo : String -> BackendTask { fatal : FatalError, recoverable : Script.Error } ()
escribeUnArchivo cuerpo =
    Script.writeFile
        { path = "src/UsuariosAuth.elm", body = cuerpo }


generandoCodigo listado =
    Elm.declaration "accesosPermitidos"
        (List.map
            Elm.string
            listado
            |> Elm.list
        )
        |> Elm.expose
        |> List.singleton
        |> Elm.file [ "UsuariosAuth" ]


allAccessDecoder : Json.Decode.Decoder (List AccessData)
allAccessDecoder =
    let
        accessDecoder =
            Json.Decode.map3 AccessData
                (Json.Decode.field "usuario" Json.Decode.string)
                (Json.Decode.field "clave" Json.Decode.string)
                (Json.Decode.field "site" Json.Decode.string)
    in
    Json.Decode.list accessDecoder
