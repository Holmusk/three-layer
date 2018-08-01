module Lib.Core.Password
       ( PasswordHash (unPwdHash)
       , PasswordPlainText (..)
       , unsafePwdHash
       , mkPasswordHashWithPolicy
       , mkPasswordHash
       , verifyPassword
       ) where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Elm (ElmType (..))

import Lib.App.Error (AppError (..), serverError, throwOnNothingM)

import qualified Crypto.BCrypt as BC

newtype PasswordHash = PwdHash { unPwdHash :: Text }
  deriving (Generic, FromField)

instance ElmType PasswordHash

instance ToJSON PasswordHash
instance FromJSON PasswordHash

unsafePwdHash :: Text -> PasswordHash
unsafePwdHash = PwdHash

newtype PasswordPlainText = PwdPlainText { unPwdPlainText :: Text }
  deriving (Show, Eq, Generic, ElmType)

instance ToJSON PasswordPlainText
instance FromJSON PasswordPlainText

-- | Generates a password hash given the hashing policy and its plane text.
-- This has to be done in IO asy generating the salt requires RNG
mkPasswordHashWithPolicy :: (MonadError AppError m, MonadIO m)
                         => BC.HashingPolicy
                         -> PasswordPlainText
                         -> m PasswordHash
mkPasswordHashWithPolicy hashPolicy password = throwOnNothingM errorMessage $ liftIO hash
  where
    hashBS = BC.hashPasswordUsingPolicy hashPolicy (encodeUtf8 $ unPwdPlainText password)
    hash = PwdHash . decodeUtf8 <<$>> hashBS
    errorMessage = serverError "Error generating password hash"

-- | Generates the password hash with slow hashing policy.
mkPasswordHash :: (MonadError AppError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash = mkPasswordHashWithPolicy BC.slowerBcryptHashingPolicy

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PwdPlainText password) (PwdHash hash) = BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
