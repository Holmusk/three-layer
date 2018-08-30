module Lib.Effects.Measure
       ( MonadMeasure (..)

         -- * Internals of 'MonadMeasure'
       , timedActionApp
       ) where

import System.CPUTime (getCPUTime)

import Lib.App (App, Has (..), Timings, grab)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

-- | Performs action
class Monad m => MonadMeasure m where
    timedAction :: Text -> m a -> m a

instance MonadMeasure App where
    timedAction = timedActionApp

-- Measure the time taken to perform the given action and store it
-- in the 'timings' distribution with the given label
timedActionApp :: forall r m a .
                  ( MonadReader r m
                  , Has Timings r
                  , Has Metrics.Store r
                  , MonadIO m
                  )
               => Text -> m a -> m a
timedActionApp label action = do
    start <- liftIO getCPUTime
    !result <- action
    end <- liftIO getCPUTime
    let !timeTaken = fromIntegral (end - start) * 1e-12
    dist <- getOrCreateDistribution
    liftIO $ Distribution.add dist timeTaken
    return result
  where
    getOrCreateDistribution :: m Distribution.Distribution
    getOrCreateDistribution = do
        timingsRef <- grab @Timings
        store <- grab @Metrics.Store
        liftIO $ do
            distMap <- readIORef timingsRef
            whenNothing (HashMap.lookup label distMap) $ do
                newDist <- Metrics.createDistribution label store
                modifyIORef' timingsRef (HashMap.insert label newDist)
                pure newDist
