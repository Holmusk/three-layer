{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module AuthSpec where

import qualified Data.Map            as Map
import qualified Data.UUID.Types     as UUID
import           Lib.App
import           Lib.Effects.Session
import           Lib.Effects.User
import           Lib.Server.Auth
import           Lib.Util.JWT
import           Protolude
import           System.Random
import           Test.Tasty
import           Test.Tasty.Hspec

newtype MockApp a = MockApp {
  unMockApp :: ReaderT AppEnv (ExceptT AppError IO) a
} deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader AppEnv, MonadIO)

instance MonadSession MockApp

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  sessions <- newMVar Map.empty
  let jwtSecret = "kjnlkjn"
  runExceptT $ runReaderT (unMockApp action) AppEnv{..}

instance MonadUser MockApp where
  getUserByEmail "test@test.com" = return . Just $ User {
      userId = UUID.nil,
      userName = "test user",
      userEmail = "test@test.com",
      -- hash of 'password'
      userHash = "$2y$10$GHIz6OuOdv3cUmU5QAPUpO7f2cmVW0b/AB4LGeRlDc4WskmzGWv5e"
  }
  getUserByEmail _ = return Nothing

spec_loginSpec :: Spec
spec_loginSpec = describe "login Handler" $ do
  it "should return a 404 on an unknown email" $
    runMockApp (loginHandler (LoginRequest "unknownemail@test.com" ""))
      `shouldReturn` Left NotFound
  it "should return a NotAllowed for a wrong password" $
    runMockApp (loginHandler (LoginRequest "test@test.com" ""))
      `shouldReturn` Left (NotAllowed "Invalid Password")
  it "should return a token for the correct password" $ do
    resp <- runMockApp (loginHandler (LoginRequest "test@test.com" "password"))
    resp `shouldSatisfy` isRight

spec_isLoggedInSpec :: Spec
spec_isLoggedInSpec = describe "isLoggedIn handler" $ do
  it "should return an error for an invalid JWT token" $
    runMockApp (isLoggedInHandler "")
      `shouldReturn` Left (NotAllowed "Invalid Token")
  it "should confirm that a valid session is valid" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest "test@test.com" "password")
      isLoggedInHandler loginResponseToken
    resp `shouldSatisfy` isRight

spec_logoutSpec :: Spec
spec_logoutSpec = describe "logout handler" $ do
  it "should be able to log out a logged in user" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest "test@test.com" "password")
      logoutHandler loginResponseToken
      isLoggedInHandler loginResponseToken
    resp `shouldBe` Left (NotAllowed "Expired Session")
