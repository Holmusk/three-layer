module Lib.Effects.Measure
       ( MonadMeasure
       , MonadTimed (timedAction)  -- temporarily required for mock tests

         -- * Internals
       , timedActionImpl
       ) where

import Relude.Extra.CallStack (ownName)
import System.CPUTime (getCPUTime)

import Lib.App (App, Has (..), Timings, grab)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

-- | Performs action
class Monad m => MonadTimed m where
    timedAction :: HasCallStack => m a -> m a

instance MonadTimed App where
    timedAction = withFrozenCallStack timedActionImpl

type MonadMeasure m = (HasCallStack, MonadTimed m)

-- | Measure the time taken to perform the given action and store it
-- in the 'timings' distribution with the given label
timedActionImpl
    :: forall r m a .
       ( MonadReader r m
       , Has Timings r
       , Has Metrics.Store r
       , MonadIO m
       , HasCallStack
       )
    => m a -> m a
timedActionImpl action = do
    start <- liftIO getCPUTime
    !result <- action
    end <- liftIO getCPUTime
    let !timeTaken = fromIntegral (end - start) * 1e-12
    dist <- getOrCreateDistribution $ toText ownName
    liftIO $ Distribution.add dist timeTaken
    return result
  where
    getOrCreateDistribution :: Text -> m Distribution.Distribution
    getOrCreateDistribution label = do
        timingsRef <- grab @Timings
        store <- grab @Metrics.Store
        liftIO $ do
            distMap <- readIORef timingsRef
            whenNothing (HashMap.lookup label distMap) $ do
                newDist <- Metrics.createDistribution label store
                modifyIORef' timingsRef (HashMap.insert label newDist)
                pure newDist
