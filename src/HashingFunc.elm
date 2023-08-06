module HashingFunc exposing (..)

import Base64.Encode as B64e
import Bytes exposing (Bytes)
import Bytes.Encode
import HmacSha1
import HmacSha1.Key as Key
import PBKDF2 exposing (pbkdf2)


--        |> BackendTask.map
--            (List.map (accessDataToString >> computeHash "JesuCristo Vive!"))


type alias AccessData =
    { usuario : String
    , clave : String
    , site : String
    }


accessDataToString : AccessData -> String
accessDataToString registro =
    registro.usuario
        ++ "%"
        ++ registro.clave
        ++ "%"
        ++ registro.site


computeHash : String -> String
computeHash password =
    let
        salt = "JesuCristo Vive!"

        p =
            Bytes.Encode.encode <| Bytes.Encode.string password

        s =
            Bytes.Encode.encode <| Bytes.Encode.string salt

        hmacSha1 : Bytes -> Bytes -> Bytes
        hmacSha1 key message =
            HmacSha1.fromBytes (Key.fromBytes key) message
                |> HmacSha1.toBytes

        showBase64 : Bytes -> String
        showBase64 bytes =
            B64e.bytes bytes
                |> B64e.encode
    in
    pbkdf2 ( hmacSha1, 12 ) p s 4096 24
        |> Result.map showBase64
        |> Result.withDefault "Error en codificar!!"
