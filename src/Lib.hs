module Lib
       ( mkAppEnv
       , runServer
       , main
       ) where

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Remote.Monitoring (forkServerWith)

import Lib.App (AppEnv, Env (..))
import Lib.Core.Jwt (JwtSecret (..), mkRandomString)
import Lib.Effects.Log (mainLogAction)
import Lib.Server (API, server)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics

mkAppEnv :: IO AppEnv
mkAppEnv = do
    let envDbPool = error "Not implemented yet"
    envSessions <- newMVar HashMap.empty
    randTxt <- mkRandomString 10
    let envJwtSecret = JwtSecret randTxt
    envTimings <- newIORef HashMap.empty
    envEkgStore <- Metrics.newStore
    let envSessionExpiry = 600
    let envLogAction = mainLogAction D
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env@Env{..} = do
    -- configure and run EKG
    Metrics.registerGcMetrics envEkgStore
    () <$ forkServerWith envEkgStore "localhost" 8081
    run 8081 application
  where
    application = serve (Proxy @API) (server env)

main :: IO ()
main = mkAppEnv >>= runServer
