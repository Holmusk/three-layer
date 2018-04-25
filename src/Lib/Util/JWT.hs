
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Lib.Util.JWT where

import           Data.Aeson            (Value (..))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.UUID.Types       (UUID)
import qualified Data.UUID.Types       as UUID
import           Lib.App.Env           (AppEnv (..))
import           Protolude
import           System.Random         (newStdGen, randomRs)
import qualified Web.JWT               as JWT

-- Make a random string comprised of a - z of
-- a given length
mkRandomString :: (MonadIO m) => Int -> m Text
mkRandomString len = do
  gen <- liftIO newStdGen
  return $ toS $ take len $ randomRs ('a', 'z') gen

newtype JWTPayload = JWTPayload {
  jwtUserId :: UUID
} deriving (Eq, Show)

jwtPayloadToMap :: JWTPayload -> Map Text Value
jwtPayloadToMap JWTPayload{..} = Map.fromList [("id", String $ UUID.toText jwtUserId)]

jwtPayloadFromMap :: Map Text Value -> Maybe JWTPayload
jwtPayloadFromMap claimsMap = do
  idVal <- Map.lookup "id" claimsMap
  mJwtId <- case idVal of
    String jwtId -> return jwtId
    _            -> Nothing
  jwtUserId <- UUID.fromText mJwtId
  return JWTPayload{..}

mkJWTToken :: (MonadIO m, MonadReader AppEnv m) => Int -> JWTPayload -> m Text
mkJWTToken expiryInSeconds payload = do
  secret <- JWT.secret <$> asks jwtSecret
  timeNow <- liftIO getPOSIXTime
  let expiryTime = timeNow + fromIntegral expiryInSeconds
  let claimsSet = JWT.def {
    JWT.exp = JWT.numericDate expiryTime,
    JWT.unregisteredClaims = jwtPayloadToMap payload
  }
  return $ JWT.encodeSigned JWT.HS256 secret claimsSet

decodeAndVerifyJWTToken :: (MonadIO m, MonadReader AppEnv m) => Text -> m (Maybe JWTPayload)
decodeAndVerifyJWTToken token = do
  secret <- JWT.secret <$> asks jwtSecret
  timeNow <- JWT.numericDate <$> liftIO getPOSIXTime
  case JWT.claims <$> JWT.decodeAndVerifySignature secret token of
    Just claimsSet ->
      case (,) <$> timeNow <*> JWT.exp claimsSet of
        Just (now, expiryTimeStatedInToken) ->
          if expiryTimeStatedInToken < now then
            return Nothing
          else
            return $ jwtPayloadFromMap $ JWT.unregisteredClaims claimsSet
    Nothing -> return Nothing
