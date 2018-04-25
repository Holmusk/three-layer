module Lib.App.Env where

import           Data.Map                   (Map)
import           Data.Pool                  (Pool)
import           Data.UUID.Types            (UUID)
import           Database.PostgreSQL.Simple (Connection)
import           Protolude

data AppEnv = AppEnv {
    dbPool    :: Pool Connection,
    sessions  :: MVar (Map UUID Session),
    jwtSecret :: Text
}

newtype Session = Session {
  isLoggedIn :: Bool
}
