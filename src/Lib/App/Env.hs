{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
       ( AppEnv (..)
       , Has (..)
       , grab

       , DbPool
       , Timings
       ) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

import System.Metrics (Store)
import System.Metrics.Distribution (Distribution)

import Lib.Core.Jwt (JwtSecret (..))
import Lib.Core.Session (SessionExpiry, Sessions)

type DbPool = Pool Connection
type Timings = IORef (HashMap Text Distribution)

data AppEnv = AppEnv
    { dbPool        :: DbPool
    , sessions      :: Sessions
    , jwtSecret     :: JwtSecret
    , timings       :: Timings
    , ekgStore      :: Store
    , sessionExpiry :: SessionExpiry
    }

{- | General type class representing which @field@ is in @env@.
Instead of plain usage like this:
@
foo = do
    secret <- asks jwtSecret
@
you should use 'Has' type class like this:
@
foo = do
    secret <- grab @JwtSecret
 -- secret <- asks $ obtain @JwtSecret
@
-}
class Has field env where
    obtain :: env -> field

instance Has DbPool        AppEnv where obtain = dbPool
instance Has Sessions      AppEnv where obtain = sessions
instance Has JwtSecret     AppEnv where obtain = jwtSecret
instance Has Timings       AppEnv where obtain = timings
instance Has Store         AppEnv where obtain = ekgStore
instance Has SessionExpiry AppEnv where obtain = sessionExpiry

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
