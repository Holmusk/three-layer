module Lib.Time
       ( -- * Protobuf related functions
         datetimeToUtc
       , utcTimeToDatetime

         -- * 'Seconds' related functions
       , Seconds (..)
       , dayInSeconds
       , threadDelay
       ) where

import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, secondsToDiffTime)

import Proto.Common (Date, Datetime, Timestamp)
import Proto.Common_Fields (date, day, month, nanos, seconds, timestamp, year)

import qualified Control.Concurrent as C (threadDelay)


----------------------------------------------------------------------------
-- Deal with protobuf
----------------------------------------------------------------------------

utcTimeToDatetime :: UTCTime -> Datetime
utcTimeToDatetime UTCTime{..} = defMessage @Datetime
    & date .~ datePart
    & timestamp .~ timePart
  where
    (dyear, dmon, dday) = toGregorian utctDay

    datePart = defMessage @Date
        & year  .~ fromIntegral dyear
        & month .~ fromIntegral dmon
        & day   .~ fromIntegral dday

    secondsInDay = diffTimeToPicoseconds utctDayTime `div` (10 ^ (12 :: Int))

    timePart = defMessage @Timestamp
        & seconds .~ fromIntegral secondsInDay
        & nanos   .~ 0

datetimeToUtc :: Datetime -> UTCTime
datetimeToUtc datetime =
    let dtYear  = toInteger $ datetime ^. date . year
        dtMonth = fromIntegral $ datetime ^. date . month
        dtDay   = fromIntegral $ datetime ^. date . day
        diffTime = secondsToDiffTime $ fromIntegral $ datetime ^. timestamp . seconds
    in UTCTime
        { utctDay = fromGregorian dtYear dtMonth dtDay
        , utctDayTime = diffTime
        }

----------------------------------------------------------------------------
-- Deal with seconds
----------------------------------------------------------------------------


-- | Represents the amount of seconds.
newtype Seconds = Seconds { unSeconds :: Int } deriving (Show, Eq)

-- | Similar to 'C.threadDelay' but receive 'Seconds' instead of 'Int'.
threadDelay :: MonadIO m => Seconds -> m ()
threadDelay (Seconds s) = liftIO $ C.threadDelay (s * 10 ^ (6 :: Int))

dayInSeconds :: Seconds
dayInSeconds = Seconds $ 60 * 60 * 24
