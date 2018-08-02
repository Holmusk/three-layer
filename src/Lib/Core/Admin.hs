module Lib.Core.Admin
       ( Admin (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Password (PasswordHash)

-- | Admin user inside Lib platform.
data Admin = Admin
    { adminId    :: Id Admin
    , adminEmail :: Email
    , adminHash  :: PasswordHash
    } deriving (Generic)

instance ToJSON Admin
instance FromJSON Admin

instance FromRow Admin where
    fromRow = do
        adminId <- field
        adminEmail <- field
        adminHash <- field
        pure Admin{..}
