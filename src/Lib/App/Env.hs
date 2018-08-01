module Lib.App.Env
       ( AppEnv (..)
       , Session (..)
       ) where

import Data.Pool (Pool)
import Data.UUID.Types (UUID)
import Database.PostgreSQL.Simple (Connection)
import System.Metrics (Store)
import System.Metrics.Distribution (Distribution)

data AppEnv = AppEnv
  { dbPool    :: Pool Connection
  , sessions  :: MVar (HashMap UUID Session)
  , jwtSecret :: Text
  , timings   :: IORef (HashMap Text Distribution)
  , ekgStore  :: Store
  }

newtype Session = Session {
  isLoggedIn :: Bool
}
