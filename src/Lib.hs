module Lib
       ( mkAppEnv
       , runServer
       , main
       ) where

import Network.Wai.Handler.Warp (run)
import System.Remote.Monitoring (forkServerWith)

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Jwt (JwtSecret (..), mkRandomString)
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction)
import Lib.Server (application)

import qualified Data.HashMap.Strict as HashMap
import qualified System.Metrics as Metrics

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool   <- initialisePool cDbCredentials
    envSessions <- newMVar HashMap.empty
    envTimings  <- newIORef HashMap.empty
    envEkgStore <- Metrics.newStore
    randTxt     <- mkRandomString 10
    let envJwtSecret = JwtSecret randTxt

    -- pure configuration
    let envSessionExpiry = 600
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env@Env{..} = do
    -- configure and run EKG
    Metrics.registerGcMetrics envEkgStore
    () <$ forkServerWith envEkgStore "localhost" 8081
    run 8080 $ application env

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
