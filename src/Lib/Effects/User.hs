{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Lib.Effects.User
       ( MonadUser (..)
       , User (..)
       ) where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID.Types (UUID)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Lib.App.Env (AppEnv)
import Lib.App.Error (AppError)
import Lib.Util.App (queryPG, timedAction)
import Lib.Util.Password (PasswordHash)

class (MonadReader AppEnv m, MonadError AppError m, MonadIO m) => MonadUser m where
  getUserByEmail :: Text -> m (Maybe User)
  getUserByEmail email = timedAction "getUserByEmail" $ safeHead <$> queryPG [sql|
    SELECT
      *
    FROM
      users
    WHERE
      email = ?
  |] [email]

data User = User
  { userId    :: UUID
  , userName  :: Text
  , userEmail :: Text
  , userHash  :: PasswordHash
  } deriving (Generic)

instance ToJSON User
instance FromJSON User

instance FromField PasswordHash

instance FromRow User where
  fromRow = do
    userId    <- field
    userName  <- field
    userEmail <- field
    userHash  <- field
    return User{..}
