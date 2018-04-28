module Main exposing (..)

import Foo exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Http exposing (..)

main : Program Never Model Msg
main = program{
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions}


-- MODEL --
type alias Model = {modelField : String, response : List String}

-- INIT --
init : (Model, Cmd Msg)
init = ({modelField = "Hello", response = []}, Cmd.none)

-- UPDATE --
type Msg = SendRequest | KeyPress String | ResponseRec (Result Http.Error SearchResponse)

sendSearchRequest : SearchRequest -> Http.Request SearchResponse
sendSearchRequest req = 
    let arg = searchRequestEncoder req
    in Http.request {
        method = "POST",
        headers = [Http.header "Content-Type" "application/x-protobuf"],
        url = "/api/search",
        body = emptyBody,
        expect = expectJson searchResponseDecoder,
        timeout = Nothing,
        withCredentials = False
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress x -> ({model | modelField = x}, Cmd.none)
        SendRequest ->
            let
                params =
                { query = model.modelField
                , pageNumber = 1
                , resultPerPage = 2
                }
            in
                (model, Http.send ResponseRec <| sendSearchRequest params)
        ResponseRec res ->
            case res of
                Err _ -> (model, Cmd.none)
                Ok r -> ({model | response = r.response}, Cmd.none)
        
-- SUBS --
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW --
view : Model -> Html Msg
view model =
    div[][
        input [onInput KeyPress] [],
        button [onClick SendRequest] [text "send"],
        div [] (List.map text model.response)
    ]
