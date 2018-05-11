module JWTSpec where

import qualified Data.UUID.Types       as UUID
import           Lib.Util.JWT
import           System.Random
import           Test.Tasty
import           Test.Tasty.QuickCheck

uuidFromWords :: (Word32, Word32, Word32, Word32) -> UUID.UUID
uuidFromWords (a,b,c,d) = UUID.fromWords a b c d

instance Arbitrary UUID.UUID where
  arbitrary = uuidFromWords <$> arbitrary
  shrink = map uuidFromWords . shrink . UUID.toWords

instance Arbitrary JWTPayload where
  arbitrary = do
    jwtUserId <- arbitrary
    return JWTPayload{..}

prop_jwtMapEncodeAndDecode :: JWTPayload -> Bool
prop_jwtMapEncodeAndDecode jwtPayload =
  let encoded = jwtPayloadToMap jwtPayload
      decoded = jwtPayloadFromMap encoded
      didDecode = isJust decoded
      isStillTheSame = fromMaybe jwtPayload decoded == jwtPayload
  in didDecode && isStillTheSame
