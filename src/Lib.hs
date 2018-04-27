{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import           Data.IORef               (newIORef)
import qualified Data.Map                 as Map
import           Lib.App
import           Lib.Server
import           Lib.Util.JWT             (mkRandomString)
import           Network.Wai.Handler.Warp
import           Protolude
import           Servant.Server
import qualified System.Metrics           as Metrics

mkAppEnv :: IO AppEnv
mkAppEnv = do
  let dbPool = undefined
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
