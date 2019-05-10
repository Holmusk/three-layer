{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Core.Jwt
       ( JwtPayload (..)
       , JwtToken (..)
       , JwtSecret (..)

         -- * 'JwtPayload' encoding decoding
       , jwtPayloadToMap
       , jwtPayloadFromMap
       , mkRandomString
       ) where

import Data.Aeson (FromJSON, ToJSON, Value (..))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (ElmType)
import System.Random (newStdGen, randomRs)
import Web.HttpApiData (FromHttpApiData)

import Lib.Core.Id (AnyId, Id (..))

import qualified Data.Map as Map
import qualified Data.UUID.Types as UUID
import qualified Web.JWT as Jwt

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype JwtSecret = JwtSecret
    { unJwtSecret :: Text
    }


newtype JwtToken = JwtToken
    { unJwtToken :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData)
      deriving anyclass (FromJSON, ToJSON, ElmType)

-- | Stores user id.
newtype JwtPayload = JwtPayload
    { unJwtPayload :: AnyId
    } deriving (Eq, Show)


-- | Makes a random string comprised of a - z of a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = toText . take len . randomRs ('a', 'z') <$> liftIO newStdGen


jwtPayloadToMap :: JwtPayload -> Jwt.ClaimsMap
jwtPayloadToMap JwtPayload{..} = Jwt.ClaimsMap $ Map.fromList
    [("id", String $ UUID.toText $ unId unJwtPayload)]

jwtPayloadFromMap :: Jwt.ClaimsMap -> Maybe JwtPayload
jwtPayloadFromMap (Jwt.ClaimsMap claimsMap) = do
    idVal <- Map.lookup "id" claimsMap
    unJwtPayload <- case idVal of
        String jwtId -> Id <$> UUID.fromText jwtId
        _            -> Nothing
    pure JwtPayload{..}
