{-# LANGUAGE KindSignatures #-}

module Main where

import Elm (ElmType (..), toElmDecoderSource, toElmEncoderSource, toElmTypeSource)
import Servant.API ((:>))
import Servant.Elm (Spec (..), generateElmForAPI, specsToDir)

import Lib.Effects.User (User)
import Lib.Server (API)

type ExportTypes = '[User]

apiSpec :: Spec
apiSpec = Spec ["Generated", "Api"]
          ( "import Http"
          : "import Json.Decode exposing (list, string)"
          : "import Json.Encode"
          : "import Generated.Types exposing (..)"
          : "import Generated.Encoder exposing (..)"
          : "import Generated.Decoder exposing (..)"
          : generateElmForAPI (Proxy :: Proxy ("api" :> API))
          )

class RenderElmType (exportTypes :: [*]) where
  renderType    :: Proxy exportTypes -> [Text]
  renderEncoder :: Proxy exportTypes -> [Text]
  renderDecoder :: Proxy exportTypes -> [Text]

instance RenderElmType '[] where
  renderType    = const []
  renderEncoder = const []
  renderDecoder = const []

instance (ElmType t, RenderElmType ts) => RenderElmType (t ': ts) where
  renderType    _ = [toElmTypeSource    $ Proxy @t] <> renderType    (Proxy @ts)
  renderEncoder _ = [toElmEncoderSource $ Proxy @t] <> renderEncoder (Proxy @ts)
  renderDecoder _ = [toElmDecoderSource $ Proxy @t] <> renderDecoder (Proxy @ts)

typeSpec :: Spec
typeSpec = Spec ["Generated", "Types"] $ renderType (Proxy @ExportTypes)

encoderSpec :: Spec
encoderSpec = Spec ["Generated", "Encoder"] $
              [ "import Json.Encode exposing (..)"
              , "import Generated.Types exposing (..)"
              ] <> renderEncoder (Proxy @ExportTypes)

decoderSpec :: Spec
decoderSpec = Spec ["Generated", "Decoder"] $
              [ "import Json.Decode exposing (..)"
              , "import Json.Decode.Pipeline exposing (..)"
              , "import Generated.Types exposing (..)"
              ] <> renderDecoder (Proxy @ExportTypes)

main :: IO ()
main = specsToDir [apiSpec, typeSpec, encoderSpec, decoderSpec] "../frontend/src/elm"
