{-# LANGUAGE FlexibleContexts #-}
module Lib.Util.Password(
  PasswordHash,
  PasswordPlainText,
  mkPasswordHash,
  verifyPassword
) where

import qualified Crypto.BCrypt as BC
import           Lib.App.Error
import           Lib.Util.App  (maybeWithM)
import           Protolude

type PasswordHash = Text
type PasswordPlainText = Text

-- Generate a password hash given its plain text. This has to be done in IO as
-- generating the salt requires RNG
mkPasswordHash :: (MonadError AppError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash password = maybeWithM errorMessage $ liftIO hashText
  where
    hash = BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy $ toS password
    hashText = (toS <$>) <$> hash
    errorMessage = ServerError "Error generating password hash"

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword password hash = BC.validatePassword (toS hash) (toS password)
