module Test.AuthSpec where

import Control.Monad.Except (MonadError)
import Katip (Katip, KatipContext)
import Katip.Monadic (NoLoggingT (..))
import Test.Tasty.Hspec

import Lib.App
import Lib.App.Error (notAllowed, notFound)
import Lib.Core.Password (PasswordPlainText (..), unsafePwdHash)
import Lib.Effects.Session
import Lib.Effects.User
import Lib.Server.Auth

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.Types as UUID
import qualified System.Metrics as Metrics

newtype MockApp a = MockApp
  { unMockApp :: ReaderT AppEnv (NoLoggingT (ExceptT AppError IO)) a
  } deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader AppEnv,
              MonadIO, Katip, KatipContext)

instance MonadSession MockApp

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  sessions <- newMVar HashMap.empty
  let jwtSecret = "kjnlkjn"
  timings  <- newIORef HashMap.empty
  ekgStore <- Metrics.newStore
  let dbPool = error "Not implemented"
  runExceptT $ runNoLoggingT $ usingReaderT AppEnv{..} $ unMockApp action

instance MonadUser MockApp where
  getUserByEmail "test@test.com" = return . Just $ User {
      userId = UUID.nil,
      userName = "test user",
      userEmail = "test@test.com",
      -- hash of 'password'
      userHash = unsafePwdHash "$2y$10$GHIz6OuOdv3cUmU5QAPUpO7f2cmVW0b/AB4LGeRlDc4WskmzGWv5e"
  }
  getUserByEmail _ = return Nothing

spec_loginSpec :: Spec
spec_loginSpec = describe "login Handler" $ do
  it "should return a 404 on an unknown email" $
    runMockApp (loginHandler (LoginRequest "unknownemail@test.com" $ PwdPlainText ""))
      `shouldReturn` Left notFound
  it "should return a notAllowed for a wrong password" $
    runMockApp (loginHandler (LoginRequest "test@test.com" $ PwdPlainText ""))
      `shouldReturn` Left (notAllowed "Invalid Password")
  it "should return a token for the correct password" $ do
    resp <- runMockApp (loginHandler (LoginRequest "test@test.com" $ PwdPlainText "password"))
    resp `shouldSatisfy` isRight

spec_isLoggedInSpec :: Spec
spec_isLoggedInSpec = describe "isLoggedIn handler" $ do
  it "should return an error for an invalid JWT token" $
    runMockApp (isLoggedInHandler "")
      `shouldReturn` Left (notAllowed "Invalid Token")
  it "should confirm that a valid session is valid" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest "test@test.com" $ PwdPlainText "password")
      isLoggedInHandler loginResponseToken
    resp `shouldSatisfy` isRight

spec_logoutSpec :: Spec
spec_logoutSpec = describe "logout handler" $
  it "should be able to log out a logged in user" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest "test@test.com" $ PwdPlainText "password")
      _ <- logoutHandler loginResponseToken
      isLoggedInHandler loginResponseToken
    resp `shouldBe` Left (notAllowed "Expired Session")
