{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Util.App(
  maybeWithM,
  maybeM,
  asSingleRow,
  queryPG,
  queryPG_,
  executePG,
  executePG_,
  executeManyPG,
  timedAction
) where

import           Control.Monad.Except        (MonadError, throwError)
import qualified Data.Map                    as Map
import qualified Data.Pool                   as Pool
import qualified Database.PostgreSQL.Simple  as PG
import           Lib.App.Env
import           Lib.App.Error
import           System.CPUTime              (getCPUTime)
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Distribution as Distribution

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

-- Measure the time taken to perform the given action and store it
-- in the 'timings' distribution with the given label
timedAction :: (MonadReader AppEnv m, MonadIO m) => Text -> m a -> m a
timedAction label action = do
  start <- liftIO getCPUTime
  !result <- action
  end <- liftIO getCPUTime
  let !timeTaken = fromIntegral (end - start) * 1e-12
  dist <- getOrCreateDistribution label
  liftIO $ Distribution.add dist timeTaken
  return result
  where
  getOrCreateDistribution :: (MonadIO m, MonadReader AppEnv m) => Text -> m Distribution.Distribution
  getOrCreateDistribution label = do
    timingsRef <- asks timings
    store <- asks ekgStore
    liftIO $ do
      distMap <- readIORef timingsRef
      whenNothing (Map.lookup label distMap) $ do
        newDist <- Metrics.createDistribution label store
        modifyIORef' timingsRef (Map.insert label newDist)
        return newDist
