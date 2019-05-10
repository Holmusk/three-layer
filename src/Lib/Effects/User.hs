{-# LANGUAGE QuasiQuotes #-}

module Lib.Effects.User
       ( MonadUser (..)
       ) where

import Lib.App (App)
import Lib.Core.Email (Email)
import Lib.Core.User (User)
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
