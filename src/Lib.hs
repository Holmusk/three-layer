{-# LANGUAGE TypeApplications #-}

module Lib where

import qualified Data.Map                 as Map
import           Lib.App
import           Lib.Server
import           Lib.Util.JWT             (mkRandomString)
import           Network.Wai.Handler.Warp
import           Servant.Server
import qualified System.Metrics           as Metrics

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
