module Test.Core.Jwt
       ( jwtMapEncodeAndDecode
       ) where

import Hedgehog (forAll, (===))

import Lib.Core.Id (Id (..))
import Lib.Core.Jwt (JwtPayload (..), jwtPayloadFromMap, jwtPayloadToMap)

import Test.Common (Test, prop)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

jwtMapEncodeAndDecode :: Test
jwtMapEncodeAndDecode = prop "jwtMapEncodeAndDecode" $ do
    randomId <- forAll $ Gen.text (Range.constant 8 25) Gen.alphaNum
    let randomJwtPayload = JwtPayload { unJwtPayload = Id randomId }
    let encoded = jwtPayloadToMap randomJwtPayload
    let decoded = jwtPayloadFromMap encoded
    decoded === Just randomJwtPayload
