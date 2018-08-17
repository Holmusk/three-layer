module Lib.Core.Session
       ( Sessions
       , Session (..)
       , SessionExpiry (..)
       , sessionExpired
       , mkNewSession
       ) where

import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import Lib.Core.Id (AnyId)


type Sessions = MVar (HashMap AnyId Session)

newtype Session = Session
    { sLoginTime :: UTCTime
    } deriving (Eq, Show)

newtype SessionExpiry = SessionExpiry
    { unSessionExpiry :: NominalDiffTime
    } deriving (Num)

-- | Checks whether session expired within given interval relative to current time
sessionExpired :: SessionExpiry -> UTCTime -> Session -> Bool
sessionExpired (SessionExpiry expiry) currentTime session =
    let sessionEnd = addUTCTime expiry $ sLoginTime session
    in sessionEnd <= currentTime

-- | Created a new 'Session'.
mkNewSession :: MonadIO m => m Session
mkNewSession = liftIO $ Session <$> getCurrentTime
