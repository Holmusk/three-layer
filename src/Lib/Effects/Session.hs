module Lib.Effects.Session
       ( MonadSession (..)

         -- * Internals of 'MonadSession'
       , getSessionImpl
       , putSessionImpl
       , deleteSessionImpl
       , isSessionExpiredImpl
       ) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Time.Clock (getCurrentTime)

import Lib.App (App (..), Has (..), grab)
import Lib.Core.Id (AnyId)
import Lib.Core.Session (Session (..), SessionExpiry (..), Sessions, sessionExpired)

import qualified Data.HashMap.Strict as HashMap


-- | Describes a monad that can provide a CRUD interface for a 'Session' type
class Monad m => MonadSession m where
    getSession       :: AnyId -> m (Maybe Session)
    putSession       :: AnyId -> Session -> m ()
    deleteSession    :: AnyId -> m ()
    isSessionExpired :: Session -> m Bool

instance MonadSession App where
    getSession       = getSessionImpl
    putSession       = putSessionImpl
    deleteSession    = deleteSessionImpl
    isSessionExpired = isSessionExpiredImpl

type WithSession r m = (MonadReader r m, Has Sessions r, Has SessionExpiry r, MonadIO m)

getSessionImpl :: WithSession r m => AnyId -> m (Maybe Session)
getSessionImpl sessionId = do
    sessionsMvar <- grab
    sessionsMap <- readMVar sessionsMvar
    pure $ HashMap.lookup sessionId sessionsMap

putSessionImpl :: WithSession r m => AnyId -> Session -> m ()
putSessionImpl sessionId newSession = do
    sessionsMvar <- grab
    liftIO $ modifyMVar_ sessionsMvar (pure . HashMap.insert sessionId newSession)

deleteSessionImpl :: WithSession r m => AnyId -> m ()
deleteSessionImpl sessionId = do
    sessionsMvar <- grab @Sessions
    liftIO $ modifyMVar_ sessionsMvar (pure . HashMap.delete sessionId)

isSessionExpiredImpl :: WithSession r m => Session -> m Bool
isSessionExpiredImpl session = do
    currentTime <- liftIO getCurrentTime
    expiry <- grab @SessionExpiry
    pure $ sessionExpired expiry currentTime session
