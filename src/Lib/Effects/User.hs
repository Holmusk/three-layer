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

import Lib.App.Env (AppEnv)
import Lib.App.Error (AppError)
import Lib.Util.App (queryPG, timedAction)
import Lib.Util.Password (PasswordHash)

import qualified Data.UUID.Types as UUID

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

instance ElmType UUID where
  toElmType = toElmType . UUID.toString

instance ToJSON User
instance FromJSON User
instance ElmType User

instance FromRow User where
  fromRow = do
    userId    <- field
    userName  <- field
    userEmail <- field
    userHash  <- field
    return User{..}
