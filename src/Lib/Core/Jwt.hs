{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib.Core.Jwt
       ( JwtPayload (..)
       , JwtToken (..)
       , jwtPayloadToMap
       , jwtPayloadFromMap
       , decodeAndVerifyJwtToken
       , mkJwtToken
       , mkRandomString
       ) where

import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Map (Map)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.Types (UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import System.Random (newStdGen, randomRs)
import Web.HttpApiData (FromHttpApiData)

import Lib.App.Env (AppEnv (..), JwtSecret (..))
import Lib.Time (Seconds (..))

import qualified Data.Map as Map
import qualified Data.UUID.Types as UUID
import qualified Web.JWT as JWT

-- Make a random string comprised of a - z of
-- a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = toText . take len . randomRs ('a', 'z') <$> liftIO newStdGen

newtype JwtPayload = JwtPayload {
  jwtUserId :: UUID
} deriving (Eq, Show)

newtype JwtToken = JwtToken { unJwtToken :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData)
    deriving anyclass (FromJSON, ToJSON)

jwtPayloadToMap :: JwtPayload -> Map Text Value
jwtPayloadToMap JwtPayload{..} = Map.fromList [("id", String $ UUID.toText jwtUserId)]

jwtPayloadFromMap :: Map Text Value -> Maybe JwtPayload
jwtPayloadFromMap claimsMap = do
  String jwtId <- Map.lookup "id" claimsMap
  jwtUserId <- UUID.fromText jwtId
  return JwtPayload{..}

mkJwtToken :: (MonadIO m, MonadReader AppEnv m) => Seconds -> JwtPayload -> m JwtToken
mkJwtToken (Seconds expiry) payload = do
  secret <- JWT.secret <$> asks (unJwtSecret . jwtSecret)
  timeNow <- liftIO getPOSIXTime
  let expiryTime = timeNow + fromIntegral expiry
  let claimsSet = JWT.def {
    JWT.exp = JWT.numericDate expiryTime,
    JWT.unregisteredClaims = jwtPayloadToMap payload
  }
  return $ JwtToken (JWT.encodeSigned JWT.HS256 secret claimsSet)

decodeAndVerifyJwtToken :: (MonadIO m, MonadReader AppEnv m) => JwtToken -> m (Maybe JwtPayload)
decodeAndVerifyJwtToken token = do
  secret <- JWT.secret <$> asks (unJwtSecret . jwtSecret)
  timeNow <- JWT.numericDate <$> liftIO getPOSIXTime
  pure $ do
    claimsSet <- JWT.claims <$> JWT.decodeAndVerifySignature secret (unJwtToken token)
    (now, expiryTimeStatedInToken) <- (,) <$> timeNow <*> JWT.exp claimsSet
    guard (expiryTimeStatedInToken >= now)
    jwtPayloadFromMap $ JWT.unregisteredClaims claimsSet
