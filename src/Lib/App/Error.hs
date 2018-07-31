module Lib.App.Error
       ( AppError (..)
       , maybeWithM
       , maybeM
       , asSingleRow
       ) where

import Control.Monad.Except (MonadError, throwError)

data AppError =
    Invalid Text
  | NotAllowed Text
  | NotFound
  | ServerError Text
  deriving (Show, Eq)

-- Extract the value from a maybe, throwing the given 'AppError' if
-- the value does not exist
maybeWithM :: (MonadError AppError m) => AppError -> m (Maybe a) -> m a
maybeWithM err action = action >>= maybe (throwError err) pure

-- Extract a value from a maybe, throwing a 'NotFound' if  the value
-- does not exist
maybeM :: (MonadError AppError m) => m (Maybe a) -> m a
maybeM = maybeWithM NotFound

-- Helper function working with results from a database when you expect
-- only one row to be returned.
asSingleRow :: (MonadError AppError m) => m [a] -> m a
asSingleRow action = maybeM (safeHead <$> action)
