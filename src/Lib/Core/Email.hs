{-# LANGUAGE DeriveAnyClass     #-}

module Lib.Core.Email
       ( Email (..)
       , emailMatchesUpidDomain
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

{- | Each provided email template encodes 2 patterns, an exact match and a
subdomain match.
>>> emailMatchesUpidDomain "subra@holmusk.com" "subra@holmusk.com"
True
>>> emailMatchesUpidDomain "subra@holmusk.com" "dmitrii@holmusk.com"
False
>>> emailMatchesUpidDomain "subra@holmusk.com" "holmusk.com"
True
>>> emailMatchesUpidDomain "subra@holmusk.com" "gmail.com"
False
-}
emailMatchesUpidDomain :: Email -> Email -> Bool
emailMatchesUpidDomain (Email candidate) (Email template)
    | Text.null template || Text.null candidate = False
    | isNothing (Text.findIndex (== '@') template) =
      Text.toLower template == Text.drop 1 (Text.dropWhile (/= '@') (Text.toLower candidate))
    | otherwise = Text.toLower template == Text.toLower candidate
