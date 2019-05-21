module Main where

import Control.Exception (bracket)
import Hedgehog (Group (..), checkParallel)
import System.IO (hSetEncoding, utf8)
import Test.Hspec (Spec, hspec)
import Test.Hspec.Core.Spec (sequential)

import Lib (mkAppEnv)
import Lib.App (AppEnv, Env (..))
import Lib.Config (loadConfig)
import Lib.Db (prepareDb)
import Lib.Effects.Log (runAppLogIO_)

import Test.Auth (authSpecs)
import Test.Common (joinSpecs)
import Test.Core.Jwt (jwtMapEncodeAndDecode)
import Test.Core.Password (pwdHashVerify)

import qualified Data.Pool as Pool


hspecTests :: AppEnv -> Spec
hspecTests = sequential . joinSpecs "Three-layer"
    [ authSpecs
    ]

hedgehogTests :: Group
hedgehogTests = Group "Roundtrip properties"
    [ pwdHashVerify         `named` "verify     . hash       ≡ True"
    , jwtMapEncodeAndDecode `named` "jwtDecode  . jwtEncode  ≡ pure"
    ]
  where
    named :: a -> b -> (b, a)
    named = flip (,)

main :: IO ()
main = bracket
    (loadConfig >>= mkAppEnv)
    (\Env{..} -> Pool.destroyAllResources envDbPool)
    runTests
  where
    runTests :: AppEnv -> IO ()
    runTests env = do
        -- fix terminal encoding
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        -- setup DB tables
        runAppLogIO_ env prepareDb

        -- run all tests
        hspec $ hspecTests env
        ifM (checkParallel hedgehogTests) exitSuccess exitFailure
