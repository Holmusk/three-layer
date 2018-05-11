{-# LANGUAGE TypeApplications #-}

module Lib
       ( mkAppEnv
       , runServer
       ) where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import Lib.App (AppEnv (..))
import Lib.Server (API, server)
import Lib.Util.JWT (mkRandomString)

import qualified Data.Map as Map
import qualified System.Metrics as Metrics

mkAppEnv :: IO AppEnv
mkAppEnv = do
  let dbPool = error "Not implemented yet"
  sessions <- newMVar Map.empty
  jwtSecret <- mkRandomString 10
  timings <- newIORef Map.empty
  ekgStore <- Metrics.newStore
  Metrics.registerGcMetrics ekgStore
  return AppEnv{..}

runServer :: AppEnv -> IO ()
runServer env = run 8080 application
  where
  application = serve (Proxy @API) (server env)
