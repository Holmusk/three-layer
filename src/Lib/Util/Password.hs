{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Util.Password
       ( PasswordHash (..)
       , PasswordPlainText (..)
       , mkPasswordHash
       , verifyPassword
       ) where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Database.PostgreSQL.Simple.FromField (FromField)

import Lib.App.Error (AppError (..))
import Lib.Util.App (maybeWithM)

import qualified Crypto.BCrypt as BC

newtype PasswordHash = PwdHash { unPwdHash :: ByteString }
  deriving (Generic, FromField)

instance ToJSON PasswordHash where
  toJSON = toJSON . decodeUtf8 @Text . unPwdHash

instance FromJSON PasswordHash where
  parseJSON = withText "pwdHash" (pure . PwdHash . encodeUtf8)

newtype PasswordPlainText = PwdPlainText { unPwdPlainText :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON PasswordPlainText
instance FromJSON PasswordPlainText

-- Generate a password hash given its plain text. This has to be done in IO as
-- generating the salt requires RNG
mkPasswordHash :: (MonadError AppError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash password = maybeWithM errorMessage $ liftIO hash
  where
    hashBS = BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy (encodeUtf8 $ unPwdPlainText password)
    hash = PwdHash <<$>> hashBS
    errorMessage = ServerError "Error generating password hash"

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PwdPlainText password) (PwdHash hash) = BC.validatePassword hash (encodeUtf8 password)
