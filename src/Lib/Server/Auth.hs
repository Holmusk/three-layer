{-# LANGUAGE TemplateHaskell #-}

module Lib.Server.Auth
       ( LoginRequest (..)
       , LoginResponse (..)
       , AuthAPI
       , authServer
       , loginHandler
       , isLoggedInHandler
       , logoutHandler
       ) where

import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Elm (ElmType)
import Katip (KatipContext, Severity (..), logTM, ls)
import Servant.API ((:>), Capture, Get, JSON, NoContent (..), Post, ReqBody)
import Servant.Generic ((:-), AsApi, AsServerT, ToServant)

import Lib.App (App, AppError (..), Session (..))
import Lib.App.Error (notAllowed, notFound, throwOnNothingM)
import Lib.Core.Jwt (JWTPayload (..), decodeAndVerifyJWTToken, mkJWTToken)
import Lib.Core.Password (PasswordPlainText (..), verifyPassword)
import Lib.Effects.Measure (timedAction)
import Lib.Effects.Session (MonadSession (..))
import Lib.Effects.User (MonadUser (..), User (..))

data LoginRequest = LoginRequest
  { loginRequestEmail    :: Text
  , loginRequestPassword :: PasswordPlainText
  } deriving (Generic, Show, Eq)

instance ElmType LoginRequest
instance FromJSON LoginRequest
instance ToJSON LoginRequest

newtype LoginResponse = LoginResponse
  { loginResponseToken :: Text
  } deriving (Generic, Show, Eq)

instance ElmType LoginResponse
instance FromJSON LoginResponse
instance ToJSON LoginResponse

data AuthSite route = AuthSite
  { -- | Login into the application, retuns a JWT if successful
    loginApp :: route :-
      "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

    -- | Check if a given JWT is valid
  , loginJWT :: route :-
      "login" :> Capture "JWT" Text :> Get '[JSON] NoContent

  , logout :: route :-
      "logout" :> Capture "JWT" Text :> Get '[JSON] NoContent
  } deriving (Generic)

type AuthAPI = ToServant (AuthSite AsApi)

authServer :: AuthSite (AsServerT App)
authServer = AuthSite
  { loginApp = loginHandler
  , loginJWT = isLoggedInHandler
  , logout   = logoutHandler
  }

loginHandler :: (MonadUser m, MonadSession m, KatipContext m) => LoginRequest -> m LoginResponse
loginHandler LoginRequest{..} = timedAction "loginHandler" $ do
  mUser <- getUserByEmail loginRequestEmail
  when (isNothing mUser) $ do
    $(logTM) DebugS $ ls $ "Given email address " <> loginRequestEmail <> " not found"
    throwError notFound
  let (Just User{..}) = mUser
  let isPasswordCorrect = verifyPassword loginRequestPassword userHash
  unless isPasswordCorrect $ do
    $(logTM) DebugS $ ls $ "Incorrect password for user " <> loginRequestEmail
    throwError (notAllowed "Invalid Password")
  putSession userId Session { isLoggedIn = True }
  token <- mkJWTToken (60 * 60 * 24) (JWTPayload userId)
  return $ LoginResponse token

isLoggedInHandler :: (MonadSession m, MonadError AppError m) => Text -> m NoContent
isLoggedInHandler token = timedAction "isLoggedInHandler" $ do
  JWTPayload{..} <- throwOnNothingM (notAllowed "Invalid Token") $ decodeAndVerifyJWTToken token
  Session{..} <- throwOnNothingM (notAllowed "Expired Session") $ getSession jwtUserId
  unless isLoggedIn $ throwError (notAllowed "Revoked Session")
  return NoContent

logoutHandler :: (MonadSession m, KatipContext m) => Text -> m NoContent
logoutHandler token = timedAction "logoutHandler" $ do
  mPayload <- decodeAndVerifyJWTToken token
  case mPayload of
    Just JWTPayload{..} -> do
      deleteSession jwtUserId
      return NoContent
    Nothing -> do
      $(logTM) DebugS $ ls $ token <> " was used to logout when it was invalid"
      return NoContent
