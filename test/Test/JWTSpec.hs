module Test.JWTSpec where

import Data.UUID.Types (UUID)
import Hedgehog (MonadGen, Property, forAll, property, (===))
import System.Random
import Test.Tasty
import Test.Tasty.Hedgehog

import Lib.Core.Jwt

import qualified Data.UUID.Types as UUID
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

test_jwtMapEncodeAndDecode :: [TestTree]
test_jwtMapEncodeAndDecode = return $ testProperty "jwtMapEncodeAndDecode" $
  property $ do
    randomId <- forAll genRandId
    let randomJwtPayload = JWTPayload { jwtUserId = randomId }
    let encoded = jwtPayloadToMap randomJwtPayload
    let decoded = jwtPayloadFromMap encoded
    decoded === Just randomJwtPayload

genRandId :: MonadGen m => m UUID
genRandId = do
  a <- genWord32
  b <- genWord32
  c <- genWord32
  d <- genWord32
  pure $ uuidFromWords (a, b, c, d)

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 (Range.constantBounded @Word32)

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a,b,c,d) = UUID.fromWords a b c d
