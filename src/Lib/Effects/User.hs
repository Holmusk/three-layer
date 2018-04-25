{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
module Lib.Effects.User where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.UUID.Types                    (UUID)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib.App.Env
import           Lib.App.Error
import           Lib.Util.App
import           Protolude

class (MonadReader AppEnv m, MonadError AppError m, MonadIO m) => MonadUser m where
  getUserByEmail :: Text -> m (Maybe User)
  getUserByEmail email = head <$> queryPG [sql|
    SELECT
      *
    FROM
      users
    WHERE
      email = ?
  |] [email]

data User = User {
  userId    :: UUID,
  userName  :: Text,
  userEmail :: Text,
  userHash  :: Text
} deriving (Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = do
    userId <- field
    userName <- field
    userEmail <- field
    userHash <- field
    return User{..}
