{-# LANGUAGE FlexibleContexts #-}

module Lib.Db
       ( queryPG
       , queryPG_
       , executePG
       , executePG_
       , executeManyPG
       ) where

import           Lib.App.Env                (AppEnv (..))

import qualified Data.Pool                  as Pool
import qualified Database.PostgreSQL.Simple as PG

-- Query the database with a given query and args and expect a list of
-- rows in return
queryPG :: (MonadReader AppEnv m, MonadIO m, PG.ToRow q, PG.FromRow r) =>
  PG.Query -> q -> m [r]
queryPG query args = do
  pool <- asks dbPool
  liftIO $ Pool.withResource pool (\conn -> PG.query conn query args)

-- Query the database with a given query and no args and expect a list
-- of rows in return
queryPG_ :: (MonadReader AppEnv m, MonadIO m, PG.FromRow r) =>
  PG.Query -> m [r]
queryPG_ query = do
  pool <- asks dbPool
  liftIO $ Pool.withResource pool (`PG.query_` query)

-- Query the database with the given query and args, returning the
-- number of rows affected
executePG :: (MonadReader AppEnv m, MonadIO m, PG.ToRow q) =>
  PG.Query -> q -> m Int64
executePG query args = do
  pool <- asks dbPool
  liftIO $ Pool.withResource pool (\conn -> PG.execute conn query args)

-- Query the database with the given query and no args, returning the
-- number of rows affected
executePG_ :: (MonadReader AppEnv m, MonadIO m) =>
  PG.Query -> m Int64
executePG_ query = do
  pool <- asks dbPool
  liftIO $ Pool.withResource pool (`PG.execute_` query)

-- Query the database many times with a given query and a list of
-- args. Returns the number of rows affected
executeManyPG :: (MonadReader AppEnv m, MonadIO m, PG.ToRow q) =>
  PG.Query -> [q] -> m Int64
executeManyPG query args = do
  pool <- asks dbPool
  liftIO $ Pool.withResource pool (\conn -> PG.executeMany conn query args)
