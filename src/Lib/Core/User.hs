{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.User
       ( User (..)
       ) where

import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Password (PasswordHash)


-- | Data type representing row in the @users@ table.
data User = User
    { userId    :: !(Id User)
    , userName  :: !Text
    , userEmail :: !Email
    , userHash  :: !PasswordHash
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON, Elm) via ElmStreet User
