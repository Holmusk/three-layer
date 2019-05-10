module Test.Auth
       ( authSpecs
       ) where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Relude.Extra.Bifunctor (firstF)
import Test.Tasty.Hspec

import Lib.App
import Lib.App.Env ()
import Lib.App.Error (notAllowed, notFound)
import Lib.Core.Email (Email (..))
import Lib.Core.Id (Id (..))
import Lib.Core.Jwt (JwtSecret (..), JwtToken (..))
import Lib.Core.Password (PasswordPlainText (..), unsafePwdHash)
import Lib.Core.User (User (..))
import Lib.Effects.Jwt
import Lib.Effects.Log (mainLogAction)
import Lib.Effects.Measure (MonadTimed (..), timedActionImpl)
import Lib.Effects.Session
import Lib.Effects.User
import Lib.Server.Auth

import Test.Common (Test, joinSpecs)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics

newtype MockApp a = MockApp
    { unMockApp :: ReaderT (Env MockApp) IO a
    } deriving (Functor, Applicative, Monad, MonadReader (Env MockApp), MonadIO)

instance MonadError AppError MockApp where
    throwError :: AppError -> MockApp a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: MockApp a -> (AppError -> MockApp a) -> MockApp a
    catchError action handler = MockApp $ ReaderT $ \env -> do
        let ioAction = usingReaderT env $ unMockApp action
        ioAction `catch` \(AppException e) -> usingReaderT env $ unMockApp $ handler e
    {-# INLINE catchError #-}

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
    envSessions <- newMVar HashMap.empty
    let envJwtSecret = JwtSecret "kjnlkjn"
    envTimings  <- newIORef HashMap.empty
    envEkgStore <- Metrics.newStore
    let envDbPool = error "Not implemented"
    let envSessionExpiry = 600
    let envLogAction = mainLogAction D
    firstF unAppException $ try $ usingReaderT Env{..} $ unMockApp action

instance MonadUser MockApp where
    getUserByEmail e@(Email "test@test.com") = pure . Just $ User
        { userId = Id ""
        , userName = "test user"
        , userEmail = e
          -- hash of 'password'
        , userHash = unsafePwdHash "$2y$10$GHIz6OuOdv3cUmU5QAPUpO7f2cmVW0b/AB4LGeRlDc4WskmzGWv5e"
        }
    getUserByEmail _ = return Nothing

instance MonadJwt MockApp where
    mkJwtToken = mkJwtTokenImpl
    decodeAndVerifyJwtToken = decodeAndVerifyJwtTokenImpl

instance MonadTimed MockApp where
    timedAction = timedActionImpl

instance MonadSession MockApp where
    getSession = getSessionImpl
    putSession = putSessionImpl
    deleteSession = deleteSessionImpl
    isSessionExpired = isSessionExpiredImpl

authSpecs :: Test
authSpecs = joinSpecs "Auth"
    [ loginSpec
    , isLoggedInSpec
    , logoutSpec
    ]

loginSpec :: Spec
loginSpec = describe "login Handler" $ do
  it "should return a 404 on an unknown email" $
    runMockApp (loginHandler (LoginRequest (Email "unknownemail@test.com") $ PasswordPlainText ""))
      `shouldReturn` Left notFound
  it "should return a notAllowed for a wrong password" $
    runMockApp (loginHandler (LoginRequest testEmail $ PasswordPlainText ""))
      `shouldReturn` Left (notAllowed "Invalid Password")
  it "should return a token for the correct password" $ do
    resp <- runMockApp (loginHandler (LoginRequest testEmail $ PasswordPlainText "password"))
    resp `shouldSatisfy` isRight

isLoggedInSpec :: Spec
isLoggedInSpec = describe "isLoggedIn handler" $ do
  it "should return an error for an invalid JWT token" $
    runMockApp (isLoggedInHandler (JwtToken ""))
      `shouldReturn` Left (notAllowed "Invalid Token")
  it "should confirm that a valid session is valid" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PasswordPlainText "password")
      isLoggedInHandler loginResponseToken
    resp `shouldSatisfy` isRight

logoutSpec :: Spec
logoutSpec = describe "logout handler" $
  it "should be able to log out a logged in user" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PasswordPlainText "password")
      _ <- logoutHandler loginResponseToken
      isLoggedInHandler loginResponseToken
    resp `shouldBe` Left (notAllowed "Expired Session")

testEmail :: Email
testEmail = Email "test@test.com"
