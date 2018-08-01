{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Lib
       ( mkAppEnv
       , runServer
       ) where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import Lib.App (AppEnv (..))
import Lib.Core.Jwt (mkRandomString)
import Lib.Server (API, server)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics

mkAppEnv :: IO AppEnv
mkAppEnv = do
  let dbPool = error "Not implemented yet"
  sessions <- newMVar HashMap.empty
  jwtSecret <- mkRandomString 10
  timings <- newIORef HashMap.empty
  ekgStore <- Metrics.newStore
  Metrics.registerGcMetrics ekgStore
  return AppEnv{..}

runServer :: AppEnv -> IO ()
runServer env = run 8080 application
  where
  application = serve (Proxy @API) (server env)
