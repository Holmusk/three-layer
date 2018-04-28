module Foo exposing (..)


import Json.Decode as JD
import Json.Encode as JE


(<$>) : (a -> b) -> JD.Decoder a -> JD.Decoder b
(<$>) =
    JD.map


(<*>) : JD.Decoder (a -> b) -> JD.Decoder a -> JD.Decoder b
(<*>) f v =
    f |> JD.andThen (\x -> x <$> v)


optionalDecoder : JD.Decoder a -> JD.Decoder (Maybe a)
optionalDecoder decoder =
    JD.oneOf
        [ JD.map Just decoder
        , JD.succeed Nothing
        ]


requiredFieldDecoder : String -> a -> JD.Decoder a -> JD.Decoder a
requiredFieldDecoder name default decoder =
    withDefault default (JD.field name decoder)


optionalFieldDecoder : String -> JD.Decoder a -> JD.Decoder (Maybe a)
optionalFieldDecoder name decoder =
    optionalDecoder (JD.field name decoder)


repeatedFieldDecoder : String -> JD.Decoder a -> JD.Decoder (List a)
repeatedFieldDecoder name decoder =
    withDefault [] (JD.field name (JD.list decoder))


withDefault : a -> JD.Decoder a -> JD.Decoder a
withDefault default decoder =
    JD.oneOf
        [ decoder
        , JD.succeed default
        ]


optionalEncoder : String -> (a -> JE.Value) -> Maybe a -> Maybe (String, JE.Value)
optionalEncoder name encoder v =
    case v of
        Just x ->
            Just ( name, encoder x )

        Nothing ->
            Nothing


requiredFieldEncoder : String -> (a -> JE.Value) -> a -> a -> Maybe ( String, JE.Value )
requiredFieldEncoder name encoder default v =
    if v == default then
        Nothing
    else
        Just ( name, encoder v )


repeatedFieldEncoder : String -> (a -> JE.Value) -> List a -> Maybe (String, JE.Value)
repeatedFieldEncoder name encoder v =
    case v of
        [] ->
            Nothing
        _ ->
            Just (name, JE.list <| List.map encoder v)


type alias SearchRequest =
    { query : String -- 1
    , pageNumber : Int -- 2
    , resultPerPage : Int -- 3
    }


searchRequestDecoder : JD.Decoder SearchRequest
searchRequestDecoder =
    JD.lazy <| \_ -> SearchRequest
        <$> (requiredFieldDecoder "query" "" JD.string)
        <*> (requiredFieldDecoder "pageNumber" 0 JD.int)
        <*> (requiredFieldDecoder "resultPerPage" 0 JD.int)


searchRequestEncoder : SearchRequest -> JE.Value
searchRequestEncoder v =
    JE.object <| List.filterMap identity <|
        [ (requiredFieldEncoder "query" JE.string "" v.query)
        , (requiredFieldEncoder "pageNumber" JE.int 0 v.pageNumber)
        , (requiredFieldEncoder "resultPerPage" JE.int 0 v.resultPerPage)
        ]


type alias SearchResponse =
    { response : List String -- 1
    }


searchResponseDecoder : JD.Decoder SearchResponse
searchResponseDecoder =
    JD.lazy <| \_ -> SearchResponse
        <$> (repeatedFieldDecoder "response" JD.string)


searchResponseEncoder : SearchResponse -> JE.Value
searchResponseEncoder v =
    JE.object <| List.filterMap identity <|
        [ (repeatedFieldEncoder "response" JE.string v.response)
        ]
