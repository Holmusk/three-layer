module Lib.Core.Password
       ( PasswordHash (unPasswordHash)
       , PasswordPlainText (..)
       , unsafePwdHash
       , mkPasswordHashWithPolicy
       , mkPasswordHash
       , verifyPassword
       ) where

import Lib.App.Error (WithError, AppErrorType, serverError, throwOnNothingM)

import qualified Crypto.BCrypt as BC

-- | Password hash.
newtype PasswordHash = PasswordHash
    { unPasswordHash :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, FromField, ToField, FromJSON, ToJSON, Elm)

-- | Unsafe function for constructing 'PasswordHash'. Used mostly for testing.
unsafePwdHash :: Text -> PasswordHash
unsafePwdHash = PasswordHash

-- | Password in plain text.
newtype PasswordPlainText = PasswordPlainText
    { unPasswordPlainText :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, FromJSON, ToJSON, Elm)


-- | Generates a password hash given the hashing policy and its plane text.
-- This has to be done in IO asy generating the salt requires RNG.
mkPasswordHashWithPolicy
    :: forall m . (WithError m, MonadIO m)
    => BC.HashingPolicy
    -> PasswordPlainText
    -> m PasswordHash
mkPasswordHashWithPolicy hashPolicy password = throwOnNothingM errorMessage hashText
  where
    hashBS :: m (Maybe ByteString)
    hashBS = liftIO $ BC.hashPasswordUsingPolicy
        hashPolicy
        (encodeUtf8 $ unPasswordPlainText password)

    hashText :: m (Maybe PasswordHash)
    hashText = PasswordHash . decodeUtf8 <<$>> hashBS

    errorMessage :: AppErrorType
    errorMessage = serverError "Error generating password hash"

-- | Generates the password hash with slow hashing policy.
mkPasswordHash :: (WithError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash = mkPasswordHashWithPolicy BC.slowerBcryptHashingPolicy

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PasswordPlainText password) (PasswordHash hash) =
    BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
