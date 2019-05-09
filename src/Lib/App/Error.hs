{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

module Lib.App.Error
       ( AppError (..)
       , AppException (..)
       , WithError
       , IError

         -- * Internal error helpers
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

         -- * Error throwing helpers
       , throwOnNothing
       , throwOnNothingM
       , notFoundOnNothing
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError, throwError)
import Servant.Server (ServerError, err401, err404, err417, err500, errBody)


-- | Type alias for errors.
type WithError m = MonadError AppError m

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

-- | App errors type.
newtype AppError = InternalError IError
    deriving (Show, Eq)

-- | App internal errors.
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

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

throwOnNothing :: WithError m => AppError -> Maybe a -> m a
throwOnNothing err = maybe (throwError err) pure

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: (WithError m) => AppError -> m (Maybe a) -> m a
throwOnNothingM err action = action >>= throwOnNothing err

-- | Similar to 'throwOnNothing' but throws a 'NotFound' if the value does not exist
notFoundOnNothing :: WithError m => Maybe a -> m a
notFoundOnNothing = throwOnNothing notFound

-- | Extract a value from a maybe, throwing a 'NotFound' if  the value
-- does not exist
notFoundOnNothingM :: (WithError m) => m (Maybe a) -> m a
notFoundOnNothingM = throwOnNothingM notFound

toHttpError :: AppError -> ServerError
toHttpError = \case
    InternalError err -> case err of
        NotFound          -> err404
        ServerError msg   -> err500 { errBody = encodeUtf8 msg }
        NotAllowed msg    -> err401 { errBody = encodeUtf8 msg }
        Invalid msg       -> err417 { errBody = encodeUtf8 msg }
        HeaderDecodeError -> err401 { errBody = "Unable to decode header" }
        JobDecodeError er -> err401 { errBody = encodeUtf8 er }
