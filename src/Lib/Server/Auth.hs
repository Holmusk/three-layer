{-# LANGUAGE DerivingVia #-}

module Lib.Server.Auth
       ( -- * API
         LoginRequest (..)
       , LoginResponse (..)
       , AuthAPI
       , authServer

         -- * Handlers
       , loginHandler
       , isLoggedInHandler
       , logoutHandler
       ) where

import Control.Monad.Except (throwError)

import Lib.App.Error (WithError, notAllowed, notFound, throwOnNothingM)
import Lib.Core.Email (Email (..))
import Lib.Core.Id (castId)
import Lib.Core.Jwt (JwtPayload (..), JwtToken (..))
import Lib.Core.Password (PasswordPlainText (..), verifyPassword)
import Lib.Core.Session (mkNewSession)
import Lib.Core.User (User (..))
import Lib.Db (WithDbPool)
import Lib.Effects.Jwt (MonadJwt (..))
import Lib.Effects.Measure (MonadMeasure, timedAction)
import Lib.Effects.Session (MonadSession (..))
import Lib.Effects.User (MonadUser (..))
import Lib.Server.Types (AppServer, ToApi)
import Lib.Time (dayInSeconds)


data LoginRequest = LoginRequest
    { loginRequestEmail    :: Email
    , loginRequestPassword :: PasswordPlainText
    } deriving (Generic, Show, Eq)
      deriving (FromJSON, ToJSON, Elm) via ElmStreet LoginRequest

newtype LoginResponse = LoginResponse
    { loginResponseToken :: JwtToken
    } deriving (Generic, Show, Eq)
      deriving newtype (FromJSON, ToJSON, Elm)

data AuthSite route = AuthSite
    { -- | Login into the application, retuns a JWT if successful
      loginRoute :: route
        :- "login"
        :> ReqBody '[JSON] LoginRequest
        :> Post '[JSON] LoginResponse

      -- | Check if a given JWT is valid
    , isLoggedInRoute :: route
        :- "login"
        :> Capture "JWT" JwtToken
        :> Get '[JSON] NoContent

    , logoutRoute :: route
        :- "logout"
        :> Capture "JWT" JwtToken
        :> Get '[JSON] NoContent
    } deriving (Generic)

type AuthAPI = ToApi AuthSite

authServer :: AuthSite AppServer
authServer = AuthSite
    { loginRoute   = loginHandler
    , isLoggedInRoute = isLoggedInHandler
    , logoutRoute     = logoutHandler
    }

loginHandler
    :: ( MonadUser m
       , MonadJwt m
       , MonadSession m
       , MonadMeasure m
       , WithDbPool env m
       , WithError m
       , WithLog env m
       )
    => LoginRequest
    -> m LoginResponse
loginHandler LoginRequest{..} = timedAction $
    getUserByEmail loginRequestEmail >>= \case
        Nothing -> do
            log D $ "Given email address " <> unEmail loginRequestEmail <> " not found"
            throwError notFound
        Just User{..} -> do
            let isPasswordCorrect = verifyPassword loginRequestPassword userHash
            unless isPasswordCorrect $ do
                log D $ "Incorrect password for user " <> unEmail loginRequestEmail
                throwError (notAllowed "Invalid Password")
            session <- mkNewSession
            let anyId = castId @() userId
            putSession anyId session
            token <- mkJwtToken dayInSeconds (JwtPayload anyId)
            pure $ LoginResponse token

isLoggedInHandler
    :: ( MonadSession m
       , MonadJwt m
       , MonadMeasure m
       , WithError m
       )
    => JwtToken
    -> m NoContent
isLoggedInHandler token = timedAction $ do
    JwtPayload{..} <- throwOnNothingM (notAllowed "Invalid Token") $ decodeAndVerifyJwtToken token
    session <- throwOnNothingM (notAllowed "Expired Session") $ getSession unJwtPayload
    whenM (isSessionExpired session) $ throwError (notAllowed "Expired Session")
    pure NoContent

logoutHandler
    :: ( MonadSession m
       , MonadMeasure m
       , MonadJwt m
       , WithLog env m
       )
    => JwtToken
    -> m NoContent
logoutHandler token = timedAction $ do
    decodeAndVerifyJwtToken token >>= \case
        Just JwtPayload{..} -> deleteSession unJwtPayload
        Nothing -> log D $ unJwtToken token <> " was used to logout when it was invalid"
    pure NoContent
