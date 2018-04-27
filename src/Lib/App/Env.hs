module Lib.App.Env where

import           Data.IORef                  (IORef)
import           Data.Map                    (Map)
import           Data.Pool                   (Pool)
import           Data.UUID.Types             (UUID)
import           Database.PostgreSQL.Simple  (Connection)
import           Protolude
import           System.Metrics              (Store)
import           System.Metrics.Distribution (Distribution)

data AppEnv = AppEnv {
    dbPool    :: Pool Connection,
    sessions  :: MVar (Map UUID Session),
    jwtSecret :: Text,
    timings   :: IORef (Map Text Distribution),
    ekgStore  :: Store
}

newtype Session = Session {
  isLoggedIn :: Bool
}
