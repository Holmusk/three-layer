{-# LANGUAGE FlexibleContexts #-}
module Lib.Util.Password(
  PasswordHash,
  PasswordPlainText,
  mkPasswordHash,
  verifyPassword
) where

import           Control.Monad.Except (MonadError)
import qualified Crypto.BCrypt        as BC
import           Lib.App.Error
import           Lib.Util.App         (maybeWithM)

type PasswordHash = Text
type PasswordPlainText = Text

-- Generate a password hash given its plain text. This has to be done in IO as
-- generating the salt requires RNG
mkPasswordHash :: (MonadError AppError m, MonadIO m) => PasswordPlainText -> m PasswordHash
mkPasswordHash password = maybeWithM errorMessage $ liftIO hashText
  where
    hash = BC.hashPasswordUsingPolicy BC.slowerBcryptHashingPolicy $ encodeUtf8 password
    hashText = decodeUtf8 <<$>> hash
    errorMessage = ServerError "Error generating password hash"

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword password hash = BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
