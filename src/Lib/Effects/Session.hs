{-# LANGUAGE FlexibleContexts #-}
module Lib.Effects.Session where

import qualified Data.Map        as Map
import           Data.UUID.Types (UUID)
import           Lib.App.Env
import           Protolude

-- class MonadSession
--  describes a monad that can provide a CRUD interface for a 'Session' type
class (MonadReader AppEnv m, MonadIO m) => MonadSession m where
  getSession :: UUID -> m (Maybe Session)
  getSession sessionId = do
    sessionsMvar <- asks sessions
    sessionsMap <- liftIO $ readMVar sessionsMvar
    return $ Map.lookup sessionId sessionsMap
  putSession :: UUID -> Session -> m ()
  putSession sessionId newSession = do
    sessionsMvar <- asks sessions
    liftIO $ modifyMVar_ sessionsMvar (return . Map.insert sessionId newSession)
  deleteSession :: UUID -> m ()
  deleteSession sessionId = do
    sessionsMvar <- asks sessions
    liftIO $ modifyMVar_ sessionsMvar (return . Map.delete sessionId)
