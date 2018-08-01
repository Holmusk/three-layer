{-# LANGUAGE ConstraintKinds #-}

module Lib.Db
       ( query
       , query_
       , execute
       , execute_
       , executeMany
       , asSingleRow
       ) where

import Lib.App.Env (AppEnv (..))
import Lib.App.Error (WithError, notFoundOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as SQL

type WithDbPool m = (MonadReader AppEnv m, MonadIO m)

-- | Query the database with a given query and args and expect a list of
-- | rows in return
query :: (WithDbPool m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> m [r]
query qx args = perform (\conn -> SQL.query conn qx args)

-- | Query the database with a given query and no args and expect a list
-- | of rows in return
query_ :: (WithDbPool m, SQL.FromRow r) => SQL.Query -> m [r]
query_ qx = perform (`SQL.query_` qx)

-- | Query the database with the given query and args, returning the
-- | number of rows affected
execute :: (WithDbPool m, SQL.ToRow q) => SQL.Query -> q -> m Int64
execute q args = perform (\conn -> SQL.execute conn q args)

-- | Query the database with the given query and no args, returning the
-- | number of rows affected
execute_ :: (WithDbPool m) => SQL.Query -> m Int64
execute_ qx = perform (`SQL.execute_` qx)

-- | Query the database many times with a given query and a list of
-- | args. Returns the number of rows affected
executeMany :: (WithDbPool m, SQL.ToRow q) => SQL.Query -> [q] -> m Int64
executeMany qx args = perform (\conn -> SQL.executeMany conn qx args)

-- | Helper function working with results from a database when you expect
-- | only one row to be returned.
asSingleRow :: (WithError m) => m [a] -> m a
asSingleRow action = notFoundOnNothingM (safeHead <$> action)

perform :: WithDbPool m => (SQL.Connection -> IO b) -> m b
perform f = do
    pool <- asks dbPool
    liftIO $ Pool.withResource pool f
