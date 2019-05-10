module Api exposing
       ( postApiLogin
       , ResultErr
       )

import Http exposing (Error)
import Json.Decode as D
import Url exposing (Url)

import Lib.Core.Encoder exposing (encodeLoginRequest)
import Lib.Core.Types exposing (LoginRequest)


type alias ResultErr a = Result Error a

postApiLogin : LoginRequest -> (ResultErr String -> msg) -> Cmd msg
postApiLogin body f = Http.request
    { method  = "POST"
    , headers = []
    , url     = "login"
    , body    = Http.jsonBody (encodeLoginRequest body)
    , expect  = Http.expectJson f D.string
    , timeout = Nothing
    , tracker = Nothing
    }
