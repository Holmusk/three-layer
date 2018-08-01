{-# LANGUAGE ConstraintKinds #-}

module Lib.App.Error
       ( AppError (..)
       , WithError
       , throwOnNothingM
       , notFoundOnNothingM
       ) where

import Control.Monad.Except (MonadError, throwError)

data AppError =
    Invalid Text
  | NotAllowed Text
  | NotFound
  | ServerError Text
  deriving (Show, Eq)

-- | Type alias for errors.
type WithError m = MonadError AppError m

-- Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
throwOnNothingM :: (WithError m) => AppError -> m (Maybe a) -> m a
throwOnNothingM err action = action >>= maybe (throwError err) pure

-- Extract a value from a maybe, throwing a 'NotFound' if  the value
-- does not exist
notFoundOnNothingM :: (WithError m) => m (Maybe a) -> m a
notFoundOnNothingM = throwOnNothingM NotFound
