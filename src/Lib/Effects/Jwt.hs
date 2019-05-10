module Lib.Effects.Jwt
       ( -- * Jwt Effect monad
         MonadJwt (..)

         -- * Internals of 'MonadJwt'
       , mkJwtTokenImpl
       , decodeAndVerifyJwtTokenImpl
       ) where

import Data.Time.Clock.POSIX (getPOSIXTime)

import Lib.App (App, Has (..), grab)
import Lib.Core.Jwt (JwtPayload, JwtSecret (..), JwtToken (..), jwtPayloadFromMap, jwtPayloadToMap)
import Lib.Time (Seconds (..))

import qualified Web.JWT as JWT

class Monad m => MonadJwt m where
    mkJwtToken :: Seconds -> JwtPayload -> m JwtToken
    decodeAndVerifyJwtToken :: JwtToken -> m (Maybe JwtPayload)

instance MonadJwt App where
    mkJwtToken = mkJwtTokenImpl
    decodeAndVerifyJwtToken = decodeAndVerifyJwtTokenImpl

mkJwtTokenImpl
    :: (MonadIO m, MonadReader r m, Has JwtSecret r)
    => Seconds -> JwtPayload -> m JwtToken
mkJwtTokenImpl (Seconds expiry) payload = do
    secret <- JWT.hmacSecret . unJwtSecret <$> grab
    timeNow <- liftIO getPOSIXTime
    let expiryTime = timeNow + fromIntegral expiry
    let claimsSet = mempty
            { JWT.exp = JWT.numericDate expiryTime
            , JWT.unregisteredClaims = jwtPayloadToMap payload
            }
    pure $ JwtToken $ JWT.encodeSigned secret claimsSet

decodeAndVerifyJwtTokenImpl
    :: (MonadIO m, MonadReader r m, Has JwtSecret r)
    => JwtToken -> m (Maybe JwtPayload)
decodeAndVerifyJwtTokenImpl (JwtToken token) = do
    secret <- JWT.hmacSecret . unJwtSecret <$> grab
    timeNow <- JWT.numericDate <$> liftIO getPOSIXTime
    pure $ do
        claimsSet <- JWT.claims <$> JWT.decodeAndVerifySignature secret token
        expiryTimeStatedInToken <- JWT.exp claimsSet
        now <- timeNow
        guard (expiryTimeStatedInToken >= now)
        jwtPayloadFromMap $ JWT.unregisteredClaims claimsSet
