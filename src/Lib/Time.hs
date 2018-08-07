module Lib.Time
       ( Seconds (..)
       , datetimeToUtc
       , utcTimeToDatetime
       , dayInSeconds
       , threadDelay
       ) where

import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, secondsToDiffTime)

import Proto.Common (Date (..), Datetime (..), Timestamp (..))
import Proto.Common_Fields (date, day, month, seconds, timestamp, year)

import qualified Control.Concurrent as C (threadDelay)

utcTimeToDatetime :: UTCTime -> Datetime
utcTimeToDatetime UTCTime{..} =
  let
    (dyear, dmon, dday) = toGregorian utctDay
    datePart = Date
      { _Date'year = fromIntegral dyear
      , _Date'month = fromIntegral dmon
      , _Date'day = fromIntegral dday
      , _Date'_unknownFields = mempty
      }
    secondsInDay = diffTimeToPicoseconds utctDayTime `div` (10 ^ (12 :: Int))
    timePart = Timestamp
      { _Timestamp'seconds = fromIntegral secondsInDay
      , _Timestamp'nanos = 0
      , _Timestamp'_unknownFields = mempty
      }
  in
    Datetime
      { _Datetime'date = datePart
      , _Datetime'timestamp = timePart
      , _Datetime'_unknownFields = mempty
      }

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

-- | Represents the amount of seconds.
newtype Seconds = Seconds { unSeconds :: Int } deriving (Show, Eq)

-- | Similar to 'C.threadDelay' but receive 'Seconds' instead of 'Int'.
threadDelay :: MonadIO m => Seconds -> m ()
threadDelay (Seconds s) = liftIO $ C.threadDelay (s * 10 ^ (6 :: Int))

dayInSeconds :: Seconds
dayInSeconds = Seconds $ 60 * 60 * 24
