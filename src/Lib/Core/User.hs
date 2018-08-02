{-# LANGUAGE DeriveAnyClass     #-}

module Lib.Core.User
       ( User (..)
       , UserDetail (..)
       , BeaconEvent (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Elm (ElmType)

import Lib.Core.Email (Email)
import Lib.Core.Id (Id)
import Lib.Core.Password (PasswordHash)

-- | Data type representing Lib user.
data User = User
    { userId                :: Id User
    , userEmail             :: Email
    , userHash              :: PasswordHash
    , userPaymentStatusCode :: Text
    , userOrganization      :: Text
    } deriving (Generic, Show, Eq)
      deriving anyclass (FromJSON, ToJSON, ElmType, ToSchema)

instance FromRow User where
    fromRow = do
       userId <- field
       userEmail <- field
       userHash <- field
       userPaymentStatusCode <- field
       userOrganization <- field
       pure User{..}

data UserDetail = UserDetail
    { userDetailId           :: Id UserDetail
    , userDetailEmail        :: Email
    , userDetailAssignedUPID :: Maybe Text
    , userDetailName         :: Text
    , userDetailCreatedAt    :: Text
    , userDetailBeaconEvents :: [BeaconEvent]
    } deriving (Generic, Show, Eq)
      deriving anyclass (FromJSON, ToJSON, ElmType, ToSchema)

data BeaconEvent = BeaconEvent
    { beaconEventAction     :: Text
    , beaconEventTimestamp  :: Text
    , beaconEventAppVersion :: Maybe Text
    } deriving (Generic, Show, Eq)
      deriving anyclass (FromJSON, ToJSON, ElmType, ToSchema)
