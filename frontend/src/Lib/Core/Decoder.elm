module Lib.Core.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Lib.Core.ElmStreet exposing (..)
import Lib.Core.Types exposing (..)


decodeAdmin : Decoder Admin
decodeAdmin = D.succeed Admin
    |> required "id" decodeId
    |> required "email" D.string
    |> required "hash" D.string

decodeUser : Decoder User
decodeUser = D.succeed User
    |> required "id" decodeId
    |> required "name" D.string
    |> required "email" D.string
    |> required "hash" D.string

decodeId : Decoder Id
decodeId = D.map Id D.string

decodeLoginRequest : Decoder LoginRequest
decodeLoginRequest = D.succeed LoginRequest
    |> required "email" D.string
    |> required "password" D.string


