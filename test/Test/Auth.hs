module Test.Auth
       ( authSpecs
       ) where

import Test.Tasty.Hspec

import Lib (mkAppEnv)
import Lib.App (App, AppError (..), AppErrorType, notAllowed, notFound, runAppAsIO)
import Lib.Core.Email (Email (..))
import Lib.Core.Jwt (JwtToken (..))
import Lib.Core.Password (PasswordPlainText (..))
import Lib.Db (prepareDb)
import Lib.Server.Auth

import Test.Assert ()
import Test.Common (Test, joinSpecs)


authSpecs :: Test
authSpecs = joinSpecs "Auth"
    [ loginSpec
    , isLoggedInSpec
    , logoutSpec
    ]

launchApp :: App a -> IO (Either AppErrorType a)
launchApp app = mkAppEnv >>= \env -> runAppAsIO env (prepareDb >> app) >>= \case
    Left AppError{..} -> pure $ Left appErrorType
    Right res -> pure $ Right res

loginSpec :: Spec
loginSpec = describe "login Handler" $ do
  it "should return a 404 on an unknown email" $
    launchApp (loginHandler (LoginRequest (Email "unknownemail@test.com") $ PasswordPlainText ""))
      `shouldReturn` Left notFound
  it "should return a notAllowed for a wrong password" $
    launchApp (loginHandler (LoginRequest testEmail $ PasswordPlainText ""))
      `shouldReturn` Left (notAllowed "Invalid Password")
  it "should return a token for the correct password" $ do
    resp <- launchApp (loginHandler (LoginRequest testEmail $ PasswordPlainText "123"))
    resp `shouldSatisfy` isRight

isLoggedInSpec :: Spec
isLoggedInSpec = describe "isLoggedIn handler" $ do
  it "should return an error for an invalid JWT token" $
    launchApp (isLoggedInHandler (JwtToken ""))
      `shouldReturn` Left (notAllowed "Invalid Token")
  it "should confirm that a valid session is valid" $ do
    resp <- launchApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PasswordPlainText "123")
      isLoggedInHandler loginResponseToken
    resp `shouldSatisfy` isRight

logoutSpec :: Spec
logoutSpec = describe "logout handler" $
  it "should be able to log out a logged in user" $ do
    resp <- launchApp $ do
      LoginResponse{..} <- loginHandler (LoginRequest testEmail $ PasswordPlainText "123")
      _ <- logoutHandler loginResponseToken
      isLoggedInHandler loginResponseToken
    resp `shouldBe` Left (notAllowed "Expired Session")

testEmail :: Email
testEmail = Email "test@test.com"
