module Test.Core.Jwt
       ( jwtMapEncodeAndDecode
       ) where

import Data.UUID.Types (UUID)
import Hedgehog (MonadGen, forAll, (===))

import Lib.Core.Id (Id (..))
import Lib.Core.Jwt (JwtPayload (..), jwtPayloadFromMap, jwtPayloadToMap)

import Test.Common (Test, prop)

import qualified Data.UUID.Types as UUID
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

jwtMapEncodeAndDecode :: Test
jwtMapEncodeAndDecode = prop "jwtMapEncodeAndDecode" $ do
    randomId <- forAll genRandId
    let randomJwtPayload = JwtPayload { unJwtPayload = Id randomId }
    let encoded = jwtPayloadToMap randomJwtPayload
    let decoded = jwtPayloadFromMap encoded
    decoded === Just randomJwtPayload

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

genRandId :: MonadGen m => m UUID
genRandId = do
    a <- genWord32
    b <- genWord32
    c <- genWord32
    d <- genWord32
    pure $ UUID.fromWords a b c d

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 (Range.constantBounded @Word32)
