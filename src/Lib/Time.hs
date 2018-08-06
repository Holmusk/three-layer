module Lib.Time
       ( Seconds (..)
       , dayInSeconds
       , threadDelay
       ) where

import qualified Control.Concurrent as C (threadDelay)

-- | Represents the amount of seconds.
newtype Seconds = Seconds
    { unSeconds :: Int
    } deriving (Show, Eq)

-- | Similar to 'C.threadDelay' but receive 'Seconds' instead of 'Int'.
threadDelay :: MonadIO m => Seconds -> m ()
threadDelay (Seconds s) = liftIO $ C.threadDelay (s * 10 ^ (6 :: Int))

dayInSeconds :: Seconds
dayInSeconds = Seconds $ 60 * 60 * 24
