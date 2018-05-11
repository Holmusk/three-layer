module JWTSpec where

import qualified Data.UUID.Types     as UUID
import           Hedgehog            (Property, forAll, property, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Lib.Util.JWT
import           System.Random
import           Test.Tasty
import           Test.Tasty.Hedgehog

test_jwtMapEncodeAndDecode :: [TestTree]
test_jwtMapEncodeAndDecode = return $ testProperty "jwtMapEncodeAndDecode" $
  property $ do
    randomId <- forAll genRandId
    let randomJwtPayload = JWTPayload { jwtUserId = randomId }
    let encoded = jwtPayloadToMap randomJwtPayload
    let decoded = jwtPayloadFromMap encoded
    decoded === Just randomJwtPayload
 where
   genRandId = fromMaybe UUID.nil . UUID.fromString <$> Gen.string (Range.linear 0 100) Gen.alphaNum
