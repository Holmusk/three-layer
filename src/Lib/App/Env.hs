{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
       ( Env (..)
       , Has (..)
       , grab

         -- * Type aliases for 'Env' fields
       , DbPool
       , Timings
       ) where

import Colog (HasLog (..), Message)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import System.Metrics (Store)
import System.Metrics.Distribution (Distribution)

import Lib.Core.Jwt (JwtSecret (..))
import Lib.Core.Session (SessionExpiry, Sessions)


type DbPool = Pool Connection
type Timings = IORef (HashMap Text Distribution)

data Env (m :: Type -> Type) = Env
    { envDbPool        ::  DbPool
    , envSessions      :: !Sessions
    , envJwtSecret     :: !JwtSecret
    , envTimings       :: !Timings
    , envEkgStore      :: !Store
    , envSessionExpiry :: !SessionExpiry
    , envLogAction     :: !(LogAction m Message)
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }

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

instance Has DbPool                (Env m) where obtain = envDbPool
instance Has Sessions              (Env m) where obtain = envSessions
instance Has JwtSecret             (Env m) where obtain = envJwtSecret
instance Has Timings               (Env m) where obtain = envTimings
instance Has Store                 (Env m) where obtain = envEkgStore
instance Has SessionExpiry         (Env m) where obtain = envSessionExpiry
instance Has (LogAction m Message) (Env m) where obtain = envLogAction

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
