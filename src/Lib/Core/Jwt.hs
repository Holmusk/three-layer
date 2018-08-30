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
import Data.Map (Map)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (ElmType)
import System.Random (newStdGen, randomRs)
import Web.HttpApiData (FromHttpApiData)

import Lib.Core.Id (AnyId, Id (..))

import qualified Data.Map as Map
import qualified Data.UUID.Types as UUID

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

newtype JwtSecret = JwtSecret { unJwtSecret :: Text }


newtype JwtToken = JwtToken { unJwtToken :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData)
    deriving anyclass (FromJSON, ToJSON, ElmType)

newtype JwtPayload = JwtPayload
    { jwtUserId :: AnyId
    } deriving (Eq, Show)


-- | Makes a random string comprised of a - z of a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = toText . take len . randomRs ('a', 'z') <$> liftIO newStdGen


jwtPayloadToMap :: JwtPayload -> Map Text Value
jwtPayloadToMap JwtPayload{..} = Map.fromList [("id", String $ UUID.toText $ unId jwtUserId)]

jwtPayloadFromMap :: Map Text Value -> Maybe JwtPayload
jwtPayloadFromMap claimsMap = do
    String jwtId <- Map.lookup "id" claimsMap
    jwtUserId <- Id <$> UUID.fromText jwtId
    pure JwtPayload{..}
