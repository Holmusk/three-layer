{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Lib.Server.Auth where

import           Control.Monad.Logger
import           Control.Monad.Except (MonadError, throwError)
import           Data.Aeson           (FromJSON, ToJSON)
import           Lib.App              (App, AppEnv (..), AppError (..),
                                       Session (..))
import           Lib.Effects.Session
import           Lib.Effects.User
import           Lib.Util.App
import           Lib.Util.JWT
import           Lib.Util.Password
import           Servant.API
import           Servant.Server

data LoginRequest = LoginRequest {
  loginRequestEmail    :: Text,
  loginRequestPassword :: Text
} deriving (Generic, Show, Eq)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

newtype LoginResponse = LoginResponse {
  loginResponseToken :: Text
} deriving (Generic, Show, Eq)

instance FromJSON LoginResponse
instance ToJSON LoginResponse

type AuthAPI =
  -- Login into the application, retuns a JWT if successful
  "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
  -- Check if a given JWT is valid
  :<|> "login" :> Capture "JWT" Text :> Get '[JSON] NoContent
  :<|> "logout" :> Capture "JWT" Text :> Get '[JSON] NoContent

authServer :: ServerT AuthAPI App
authServer = loginHandler :<|> isLoggedInHandler :<|> logoutHandler

loginHandler :: (MonadUser m, MonadSession m, MonadLogger m) => LoginRequest -> m LoginResponse
loginHandler LoginRequest{..} = timedAction "loginHandler" $ do
  mUser <- getUserByEmail loginRequestEmail
  when (isNothing mUser) $ do
    $(logDebug) $ "Given email address " <> loginRequestEmail <> " not found"
    throwError NotFound
  let (Just User{..}) = mUser
  let isPasswordCorrect = verifyPassword loginRequestPassword userHash
  unless isPasswordCorrect $ do
    $(logDebug) $ "Incorrect password for user " <> loginRequestEmail
    throwError (NotAllowed "Invalid Password")
  putSession userId Session { isLoggedIn = True }
  token <- mkJWTToken (60 * 60 * 24) (JWTPayload userId)
  return $ LoginResponse token

isLoggedInHandler :: (MonadSession m, MonadError AppError m) => Text -> m NoContent
isLoggedInHandler token = timedAction "isLoggedInHandler" $ do
  JWTPayload{..} <- maybeWithM (NotAllowed "Invalid Token") $ decodeAndVerifyJWTToken token
  Session{..} <- maybeWithM (NotAllowed "Expired Session") $ getSession jwtUserId
  unless isLoggedIn $ throwError (NotAllowed "Revoked Session")
  return NoContent

logoutHandler :: (MonadSession m, MonadLogger m) => Text -> m NoContent
logoutHandler token = timedAction "logoutHandler" $ do
  mPayload <- decodeAndVerifyJWTToken token
  case mPayload of
    Just JWTPayload{..} -> do
      deleteSession jwtUserId
      return NoContent
    Nothing -> do
      $(logDebug) $ token <> " was used to logout when it was invalid"
      return NoContent
