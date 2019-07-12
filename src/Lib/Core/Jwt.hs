module Lib.Core.Jwt
       ( JwtPayload (..)
       , JwtToken (..)
       , JwtSecret (..)

         -- * 'JwtPayload' encoding decoding
       , jwtPayloadToMap
       , jwtPayloadFromMap
       , mkRandomString
       ) where

import Data.Aeson (Value (..))
import System.Random (newStdGen, randomRs)

import Lib.Core.Id (AnyId, Id (..))

import qualified Data.Map as Map
import qualified Web.JWT as Jwt


newtype JwtSecret = JwtSecret
    { unJwtSecret :: Text
    }


newtype JwtToken = JwtToken
    { unJwtToken :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData, FromJSON, ToJSON, Elm)

-- | Stores user id.
newtype JwtPayload = JwtPayload
    { unJwtPayload :: AnyId
    } deriving stock (Eq, Show)


-- | Makes a random string comprised of a - z of a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = toText . take len . randomRs ('a', 'z') <$> liftIO newStdGen


jwtPayloadToMap :: JwtPayload -> Jwt.ClaimsMap
jwtPayloadToMap JwtPayload{..} = Jwt.ClaimsMap $ Map.fromList
    [("id", String $ unId unJwtPayload)]

jwtPayloadFromMap :: Jwt.ClaimsMap -> Maybe JwtPayload
jwtPayloadFromMap (Jwt.ClaimsMap claimsMap) = do
    idVal <- Map.lookup "id" claimsMap
    unJwtPayload <- case idVal of
        String jwtId -> pure $ Id jwtId
        _            -> Nothing
    pure JwtPayload{..}
