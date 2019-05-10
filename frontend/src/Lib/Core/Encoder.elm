module Lib.Core.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Lib.Core.ElmStreet exposing (..)
import Lib.Core.Types exposing (..)


encodeAdmin : Admin -> Value
encodeAdmin x = E.object
    [ ("id", encodeId x.id)
    , ("email", E.string x.email)
    , ("hash", E.string x.hash)
    ]

encodeUser : User -> Value
encodeUser x = E.object
    [ ("id", encodeId x.id)
    , ("name", E.string x.name)
    , ("email", E.string x.email)
    , ("hash", E.string x.hash)
    ]

encodeId : Id -> Value
encodeId x = E.string x.unId




