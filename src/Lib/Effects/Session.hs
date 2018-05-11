{-# LANGUAGE FlexibleContexts #-}

module Lib.Effects.Session
       ( MonadSession (..)
       ) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.UUID.Types (UUID)

import Lib.App.Env (AppEnv (..), Session)

import qualified Data.Map as Map

-- class MonadSession
--  describes a monad that can provide a CRUD interface for a 'Session' type
class (MonadReader AppEnv m, MonadIO m) => MonadSession m where
  getSession :: UUID -> m (Maybe Session)
  getSession sessionId = do
    sessionsMvar <- asks sessions
    sessionsMap  <- readMVar sessionsMvar
    return $ Map.lookup sessionId sessionsMap

  putSession :: UUID -> Session -> m ()
  putSession sessionId newSession = do
    sessionsMvar <- asks sessions
    liftIO $ modifyMVar_ sessionsMvar (return . Map.insert sessionId newSession)

  deleteSession :: UUID -> m ()
  deleteSession sessionId = do
    sessionsMvar <- asks sessions
    liftIO $ modifyMVar_ sessionsMvar (return . Map.delete sessionId)
