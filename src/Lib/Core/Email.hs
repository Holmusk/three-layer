{-# LANGUAGE DeriveAnyClass     #-}

module Lib.Core.Email
       ( Email (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema, ToSchema)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Elm (ElmType)

import qualified Data.Text as Text

newtype Email = Email { unEmail :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField)
    deriving anyclass (FromJSON, ToJSON, ElmType, ToSchema, ToParamSchema)
