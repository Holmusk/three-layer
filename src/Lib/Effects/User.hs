{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Effects.User
       ( MonadUser (..)
       , User (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Elm (ElmType)

import Lib.App (App)
import Lib.Core.Password (PasswordHash)
import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Db (WithDbPool, query)
import Lib.Effects.Measure (MonadMeasure, timedAction)


class MonadUser m where
    getUserByEmail :: Email -> m (Maybe User)

instance MonadUser App where
    getUserByEmail = getUserByEmailImpl

getUserByEmailImpl :: (WithDbPool env m, MonadMeasure m) => Email -> m (Maybe User)
getUserByEmailImpl email = timedAction $ viaNonEmpty head <$> query [sql|
    SELECT
      *
    FROM
      users
    WHERE
      email = ?
  |] [email]

-- TODO: to be removed?
data User = User
    { userId    :: Id User
    , userName  :: Text
    , userEmail :: Email
    , userHash  :: PasswordHash
    } deriving (Generic)
      deriving anyclass (FromJSON, ToJSON, ElmType)

instance FromRow User where
    fromRow = do
        userId    <- field
        userName  <- field
        userEmail <- field
        userHash  <- field
        return User{..}
