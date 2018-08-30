module Lib.Effects.Session
       ( MonadSession (..)
         -- * Internals of 'MonadSession'
       , getSessionApp
       , putSessionApp
       , deleteSessionApp
       , isSessionExpiredApp
       ) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Time.Clock (getCurrentTime)

import Lib.App (App (..), Has (..), grab)
import Lib.Core.Id (AnyId)
import Lib.Core.Session (Session (..), SessionExpiry (..), Sessions, sessionExpired)

import qualified Data.HashMap.Strict as HashMap

-- class MonadSession
--  describes a monad that can provide a CRUD interface for a 'Session' type

-- class MonadSession
--  describes a monad that can provide a CRUD interface for a 'Session' type
class Monad m => MonadSession m where
    getSession       :: AnyId -> m (Maybe Session)
    putSession       :: AnyId -> Session -> m ()
    deleteSession    :: AnyId -> m ()
    isSessionExpired :: Session -> m Bool

instance MonadSession App where
    getSession       = getSessionApp
    putSession       = putSessionApp
    deleteSession    = deleteSessionApp
    isSessionExpired = isSessionExpiredApp

type WithSession r m = (MonadReader r m, Has Sessions r, Has SessionExpiry r, MonadIO m)

getSessionApp :: WithSession r m => AnyId -> m (Maybe Session)
getSessionApp sessionId = do
    sessionsMvar <- grab
    sessionsMap <- readMVar sessionsMvar
    pure $ HashMap.lookup sessionId sessionsMap

putSessionApp :: WithSession r m => AnyId -> Session -> m ()
putSessionApp sessionId newSession = do
    sessionsMvar <- grab
    liftIO $ modifyMVar_ sessionsMvar (pure . HashMap.insert sessionId newSession)

deleteSessionApp :: WithSession r m => AnyId -> m ()
deleteSessionApp sessionId = do
    sessionsMvar <- grab @Sessions
    liftIO $ modifyMVar_ sessionsMvar (pure . HashMap.delete sessionId)

isSessionExpiredApp :: WithSession r m => Session -> m Bool
isSessionExpiredApp session = do
    currentTime <- liftIO getCurrentTime
    expiry <- grab @SessionExpiry
    pure $ sessionExpired expiry currentTime session
