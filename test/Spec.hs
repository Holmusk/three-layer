module Main where

import Test.Auth (authSpecs)
import Test.Core.Jwt (jwtMapEncodeAndDecode)
import Test.Core.Password (pwdHashVerify)

import qualified Test.Tasty as T

testTree :: IO T.TestTree
testTree = T.testGroup "Tests" <$> sequence
    [ -- spec
      authSpecs
      -- Properties
    , jwtMapEncodeAndDecode
    , pwdHashVerify
    ]

main :: IO ()
main = testTree >>= T.defaultMainWithIngredients T.defaultIngredients
