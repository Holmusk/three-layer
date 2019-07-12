{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.Admin
       ( Admin (..)
       ) where

import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Password (PasswordHash)


-- | Admin user inside platform. Represents row in the @admin@ table.
data Admin = Admin
    { adminId    :: !(Id Admin)
    , adminEmail :: !Email
    , adminHash  :: !PasswordHash
    } deriving stock (Generic)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON, Elm) via ElmStreet Admin
