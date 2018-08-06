module Lib.Core.Jwt
       ( JwtPayload (..)
       , jwtPayloadToMap
       , jwtPayloadFromMap
       , decodeAndVerifyJwtToken
       , mkJwtToken
       , mkRandomString
       ) where

import Data.Aeson (Value (..))
import Data.Map (Map)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.Types (UUID)
import System.Random (newStdGen, randomRs)

import Lib.App.Env (AppEnv (..))
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

jwtPayloadToMap :: JwtPayload -> Map Text Value
jwtPayloadToMap JwtPayload{..} = Map.fromList [("id", String $ UUID.toText jwtUserId)]

jwtPayloadFromMap :: Map Text Value -> Maybe JwtPayload
jwtPayloadFromMap claimsMap = do
  String jwtId <- Map.lookup "id" claimsMap
  jwtUserId <- UUID.fromText jwtId
  return JwtPayload{..}

mkJwtToken :: (MonadIO m, MonadReader AppEnv m) => Seconds -> JwtPayload -> m Text
mkJwtToken (Seconds expiry) payload = do
  secret <- JWT.secret <$> asks jwtSecret
  timeNow <- liftIO getPOSIXTime
  let expiryTime = timeNow + fromIntegral expiry
  let claimsSet = JWT.def {
    JWT.exp = JWT.numericDate expiryTime,
    JWT.unregisteredClaims = jwtPayloadToMap payload
  }
  return $ JWT.encodeSigned JWT.HS256 secret claimsSet

decodeAndVerifyJwtToken :: (MonadIO m, MonadReader AppEnv m) => Text -> m (Maybe JwtPayload)
decodeAndVerifyJwtToken token = do
  secret <- JWT.secret <$> asks jwtSecret
  timeNow <- JWT.numericDate <$> liftIO getPOSIXTime
  pure $ do
    claimsSet <- JWT.claims <$> JWT.decodeAndVerifySignature secret token
    (now, expiryTimeStatedInToken) <- (,) <$> timeNow <*> JWT.exp claimsSet
    guard (expiryTimeStatedInToken >= now)
    jwtPayloadFromMap $ JWT.unregisteredClaims claimsSet
