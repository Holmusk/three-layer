{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Effects.User
       ( MonadUser (..)
       , User (..)
       ) where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID.Types (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Elm (ElmType (..))

import Lib.App (App)
import Lib.App.Env (AppEnv)
import Lib.App.Error (AppError)
import Lib.Core.Password (PasswordHash)
import Lib.Db (query)
import Lib.Effects.Measure (timedAction)

import qualified Data.UUID.Types as UUID

class (MonadReader AppEnv m, MonadError AppError m, MonadIO m) => MonadUser m where
  getUserByEmail :: Text -> m (Maybe User)
  getUserByEmail email = timedAction "getUserByEmail" $ safeHead <$> query [sql|
    SELECT
      *
    FROM
      users
    WHERE
      email = ?
  |] [email]

instance MonadUser App

data User = User
  { userId    :: UUID
  , userName  :: Text
  , userEmail :: Text
  , userHash  :: PasswordHash
  } deriving (Generic)

instance ElmType UUID where
  toElmType = toElmType . UUID.toString

instance FromJSON User
instance ElmType User
instance ToJSON User

instance FromRow User where
  fromRow = do
    userId    <- field
    userName  <- field
    userEmail <- field
    userHash  <- field
    return User{..}
