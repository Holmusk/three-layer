{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE PatternSynonyms #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Colog
       , module Control.Lens
       , module Json
       , module Elm
       , module Proto
       , module Sql
       , module Web

       , WithLog
       ) where

-- Reexport
import Relude

import Control.Lens ((.~), (^.))

import Colog (pattern D, pattern E, pattern I, LogAction (..), Severity (..), pattern W, log)

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))

import Data.ProtoLens.Message as Proto (defMessage)

import Elm (Elm (..), ElmStreet (..), elmStreetParseJson, elmStreetToJson)

import Database.PostgreSQL.Simple.FromField as Sql (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow as Sql (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ as Sql (sql)
import Database.PostgreSQL.Simple.ToField as Sql (ToField (toField))
import Database.PostgreSQL.Simple.ToRow as Sql (ToRow (toRow))
import Database.PostgreSQL.Simple.Types as Sql (Only (..))
import PgNamed as Sql ((=?))

import Servant.API as Web ((:>), Capture, Get, Header, Header', JSON, NoContent (NoContent), Post,
                           QueryParam, QueryParam', ReqBody)
import Servant.API.ContentTypes.Proto as Web (Proto)
import Servant.API.Generic as Web ((:-), toServant)
import Web.HttpApiData as Web (FromHttpApiData (..), ToHttpApiData (..))

-- Internal
import qualified Colog (Message, WithLog)


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m
