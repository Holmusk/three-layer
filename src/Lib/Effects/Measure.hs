{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Effects.Measure (timedAction) where

import           System.CPUTime              (getCPUTime)

import           Lib.App                     (AppEnv, ekgStore, timings)

import qualified Data.HashMap.Strict         as HashMap
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Distribution as Distribution

-- | Performs action
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
  getOrCreateDistribution lbl = do
    timingsRef <- asks timings
    store <- asks ekgStore
    liftIO $ do
      distMap <- readIORef timingsRef
      whenNothing (HashMap.lookup lbl distMap) $ do
        newDist <- Metrics.createDistribution lbl store
        modifyIORef' timingsRef (HashMap.insert lbl newDist)
        return newDist
