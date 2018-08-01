{-# LANGUAGE ConstraintKinds #-}

module Lib.App.Error
       ( AppError (..)
       , WithError
       , throwOnNothingM
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError, throwError)

data AppError = InternalError IError deriving (Show, Eq)

-- | Type alias for errors.
type WithError m = MonadError AppError m

-- | Extract the value from a maybe, throwing the given 'AppError' if
-- | the value does not exist
throwOnNothingM :: (WithError m) => AppError -> m (Maybe a) -> m a
throwOnNothingM err action = action >>= maybe (throwError err) pure

-- | Extract a value from a maybe, throwing a 'NotFound' if  the value
-- | does not exist
notFoundOnNothingM :: (WithError m) => m (Maybe a) -> m a
notFoundOnNothingM = throwOnNothingM NotFound

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
