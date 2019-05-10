module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, button)
import Html.Attributes exposing (src, class, disabled)
import Html.Events exposing (onClick)

import Api exposing (ResultErr, postApiLogin)
import Lib.Core.Types exposing (OneType)

---- MODEL ----

type alias Model =
    { user       : Maybe LoginRequest
    , getErr     : Bool
    }

init : ( Model, Cmd Msg )
init = ({user = Nothing, getErr = False}, Cmd.none)

---- UPDATE ----

type Msg
    = NoOp
    | Login
    | LoginRes (ResultErr String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    NoOp -> ( model, Cmd.none )
    Login -> (model, getOneType GetOneTypeRes)
    LoginRes res -> case res of
        Ok oneType -> ({model| user = Just oneType}, Cmd.none)
        Err err -> ({model| getErr = True}, Cmd.none)

---- VIEW ----

view : Model -> Html Msg
view m = div []
    [ h1 [] [text "Elm Street testing application"]
    , h2 [] [text "Get 'OneType' endpoint"]
    , button [onClick GetOneType] [text "Get OneType"]
    , div [class "err"] (if m.getErr then [text "Get errored"] else [])
    , h2 [] [text "Get 'OneType' endpoint"]
    , button [onClick (PostOneType m.oneType), disabled (isNothing m.oneType)] [text "Post OneType"]
    , div [class "err"] (if m.postErr then [text "Post errored"] else [])
    , div [] <| case m.postResult of
         Just True -> [text "The 'get' and 'post' OneType is the same :)"]
         Just False -> [text "The 'get' and 'post' OneType are different :("]
         Nothing -> []
    , button [onClick Refresh] [text "Refresh"]
    ]

---- PROGRAM ----

main : Program () Model Msg
main = Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }

-- Util
isNothing : Maybe a -> Bool
isNothing x = case x of
    Nothing -> True
    _ -> False
