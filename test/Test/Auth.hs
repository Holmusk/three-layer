module Test.Auth
       ( authSpecs
       ) where

import Control.Monad.Except (MonadError)
import Katip (Katip, KatipContext)
import Katip.Monadic (NoLoggingT (..))
import Test.Tasty.Hspec

import Lib.App
import Lib.App.Error (notAllowed, notFound)
import Lib.Core.Email (Email (..))
import Lib.Core.Id (Id (..))
import Lib.Core.Jwt (JwtSecret (..), JwtToken (..))
import Lib.Core.Password (PasswordPlainText (..), unsafePwdHash)
import Lib.Effects.Jwt
import Lib.Effects.Measure (MonadTimed (..), timedActionImpl)
import Lib.Effects.Session
import Lib.Effects.User
import Lib.Server.Auth

import Test.Common (Test, joinSpecs)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.Types as UUID
import qualified System.Metrics as Metrics

newtype MockApp a = MockApp
  { unMockApp :: ReaderT AppEnv (NoLoggingT (ExceptT AppError IO)) a
  } deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader AppEnv,
              MonadIO, Katip, KatipContext)

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  sessions <- newMVar HashMap.empty
  let jwtSecret = JwtSecret "kjnlkjn"
  timings  <- newIORef HashMap.empty
  ekgStore <- Metrics.newStore
  let dbPool = error "Not implemented"
  let sessionExpiry = 600
  runExceptT $ runNoLoggingT $ usingReaderT AppEnv{..} $ unMockApp action

instance MonadUser MockApp where
  getUserByEmail e@(Email "test@test.com") = pure . Just $ User {
      userId = Id UUID.nil,
      userName = "test user",
      userEmail = e,
      -- hash of 'password'
      userHash = unsafePwdHash "$2y$10$GHIz6OuOdv3cUmU5QAPUpO7f2cmVW0b/AB4LGeRlDc4WskmzGWv5e"
  }
  getUserByEmail _ = return Nothing

instance MonadJwt MockApp where
    mkJwtToken = mkJwtTokenApp
    decodeAndVerifyJwtToken = decodeAndVerifyJwtTokenApp

instance MonadTimed MockApp where
    timedAction = timedActionImpl

instance MonadSession MockApp where
    getSession = getSessionApp
    putSession = putSessionApp
    deleteSession = deleteSessionApp
    isSessionExpired = isSessionExpiredApp

authSpecs :: Test
authSpecs = joinSpecs "Auth"
    [ loginSpec
    , isLoggedInSpec
    , logoutSpec
    ]

loginSpec :: Spec
loginSpec = describe "login Handler" $ do
  it "should return a 404 on an unknown email" $
    runMockApp (loginHandler (LoginRequest (Email "unknownemail@test.com") $ PwdPlainText ""))
      `shouldReturn` Left notFound
  it "should return a notAllowed for a wrong password" $
    runMockApp (loginHandler (LoginRequest testEmail $ PwdPlainText ""))
      `shouldReturn` Left (notAllowed "Invalid Password")
  it "should return a token for the correct password" $ do
    resp <- runMockApp (loginHandler (LoginRequest testEmail $ PwdPlainText "password"))
    resp `shouldSatisfy` isRight

isLoggedInSpec :: Spec
isLoggedInSpec = describe "isLoggedIn handler" $ do
  it "should return an error for an invalid JWT token" $
    runMockApp (isLoggedInHandler (JwtToken ""))
      `shouldReturn` Left (notAllowed "Invalid Token")
  it "should confirm that a valid session is valid" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PwdPlainText "password")
      isLoggedInHandler loginResponseToken
    resp `shouldSatisfy` isRight

logoutSpec :: Spec
logoutSpec = describe "logout handler" $
  it "should be able to log out a logged in user" $ do
    resp <- runMockApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PwdPlainText "password")
      _ <- logoutHandler loginResponseToken
      isLoggedInHandler loginResponseToken
    resp `shouldBe` Left (notAllowed "Expired Session")

testEmail :: Email
testEmail = Email "test@test.com"
