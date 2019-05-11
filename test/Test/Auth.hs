module Test.Auth
       ( authSpecs
       ) where

import Test.Hspec (Spec, describe, it)

import Lib.App (AppEnv, WithError, notAllowed)
import Lib.App.Env ()
import Lib.Core.Email (Email (..))
import Lib.Core.Jwt (JwtToken (..))
import Lib.Core.Password (PasswordPlainText (..))
import Lib.Db (WithDb, singleRowError)
import Lib.Effects.Jwt (MonadJwt)
import Lib.Effects.Measure (MonadMeasure)
import Lib.Effects.Session (MonadSession)
import Lib.Server.Auth

import Test.Assert (failsWith, succeeds)
import Test.Common (joinSpecs)


authSpecs :: AppEnv -> Spec
authSpecs = joinSpecs "Auth"
    [ loginSpec
    , isLoggedInSpec
    , logoutSpec
    ]

loginSpec :: AppEnv -> Spec
loginSpec env = describe "login Handler" $ do
    it "should return a 404 on an unknown email" $
        env & tryLog "unknownemail@test.com" "" `failsWith` singleRowError
    it "should return a notAllowed for a wrong password" $
        env & tryLog testEmail "" `failsWith` notAllowed "Invalid Password"
    it "should return a token for the correct password" $
        env & succeeds (tryLog testEmail "123")

isLoggedInSpec :: AppEnv -> Spec
isLoggedInSpec env = describe "isLoggedIn handler" $ do
    it "should return an error for an invalid JWT token" $
      env & isLoggedInHandler (JwtToken "") `failsWith` notAllowed "Invalid Token"
    it "should confirm that a valid session is valid" $
        env & succeeds
            (tryLog testEmail "123" >>= \LoginResponse{..} -> isLoggedInHandler loginResponseToken)

logoutSpec :: AppEnv -> Spec
logoutSpec env = describe "logout handler" $
    it "should be able to log out a logged in user" $
        env & ( tryLog testEmail "123"
              >>= \LoginResponse{..} -> logoutHandler loginResponseToken
              >> isLoggedInHandler loginResponseToken
              ) `failsWith` notAllowed "Expired Session"

testEmail :: Text
testEmail = "test@test.com"

tryLog
    :: ( MonadJwt m
       , MonadSession m
       , MonadMeasure m
       , WithDb env m
       , WithError m
       , WithLog env m
       )
    => Text -> Text -> m LoginResponse
tryLog email pwd = loginHandler (LoginRequest (Email email) $ PasswordPlainText pwd)
