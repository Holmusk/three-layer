{-# LANGUAGE DeriveAnyClass     #-}

module Lib.Core.Email
       ( Email (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (ElmType)

newtype Email = Email { unEmail :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField)
    deriving anyclass (FromJSON, ToJSON, ElmType)
