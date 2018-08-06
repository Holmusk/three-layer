module Lib.App.Env
       ( AppEnv (..)
       , JwtSecret (..)
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
  , jwtSecret :: JwtSecret
  , timings   :: IORef (HashMap Text Distribution)
  , ekgStore  :: Store
  }

newtype Session = Session {
  isLoggedIn :: Bool
}

newtype JwtSecret = JwtSecret {
  unJwtSecret :: Text
}
