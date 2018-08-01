{-# LANGUAGE ConstraintKinds #-}

module Lib.App.Error
       ( AppError (..)
       , WithError
       , IError
       , throwOnNothingM
       , notFoundOnNothingM
       , isServerError
       , isNotAllowed
       , isInvalid
       , notFound
       , serverError
       , notAllowed
       , invalid
       , headerDecodeError
       , jobDecodeError
       , toHttpError
       ) where

import Control.Monad.Except (MonadError, throwError)
import Servant.Server (ServantErr, err401, err404, err417, err500, errBody)

newtype AppError = InternalError IError deriving (Show, Eq)

data IError =
    -- | General not found
      NotFound
    -- | Some exceptional circumstance has happened
    -- stop execution and return. Optional text to
    -- provide some context in server logs
    | ServerError Text
    -- | A required permission level was not met.
    -- Optional text to provide some context.
    | NotAllowed Text
    -- | Given inputs do not conform to the expected
    -- format or shape. Optional text to
    -- provide some context in server logs
    | Invalid Text
    -- | An authentication header that was required
    -- was provided but not in a format that the server
    -- can understand
    | HeaderDecodeError
    -- | Failed to parse 'CDMP.Effects.Job.Job' from 'Text'.
    | JobDecodeError Text
    deriving (Show, Eq)

isServerError :: AppError -> Bool
isServerError (InternalError (ServerError _)) = True
isServerError _                               = False

isNotAllowed :: AppError -> Bool
isNotAllowed (InternalError (NotAllowed _)) = True
isNotAllowed _                              = False

isInvalid :: AppError -> Bool
isInvalid (InternalError (Invalid _)) = True
isInvalid _                           = False

----------------------------------------------------------------------------
-- Internal Error helpers
----------------------------------------------------------------------------

notFound :: AppError
notFound = InternalError NotFound

serverError :: Text -> AppError
serverError = InternalError . ServerError

notAllowed :: Text -> AppError
notAllowed = InternalError . NotAllowed

invalid :: Text -> AppError
invalid = InternalError . Invalid

headerDecodeError :: AppError
headerDecodeError = InternalError HeaderDecodeError

jobDecodeError :: Text -> AppError
jobDecodeError = InternalError . JobDecodeError

-- | Type alias for errors.
type WithError m = MonadError AppError m

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: (WithError m) => AppError -> m (Maybe a) -> m a
throwOnNothingM err action = action >>= maybe (throwError err) pure

-- | Extract a value from a maybe, throwing a 'NotFound' if  the value
-- does not exist
notFoundOnNothingM :: (WithError m) => m (Maybe a) -> m a
notFoundOnNothingM = throwOnNothingM (InternalError NotFound)

toHttpError :: AppError -> ServantErr
toHttpError = \case
  InternalError err ->
    case err of
      NotFound          -> err404
      ServerError msg   -> err500 { errBody = encodeUtf8 msg }
      NotAllowed msg    -> err401 { errBody = encodeUtf8 msg }
      Invalid msg       -> err417 { errBody = encodeUtf8 msg }
      HeaderDecodeError -> err401 { errBody = "Unable to decode header" }
      JobDecodeError er -> err401 { errBody = encodeUtf8 er }
