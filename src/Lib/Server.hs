{-# LANGUAGE TypeOperators #-}
module Lib.Server where

import           Lib.App
import           Lib.Server.Auth
import           Servant.Server

type API = AuthAPI

server :: AppEnv -> Server API
server env = enter transform combinedServers
  where
  combinedServers = authServer
  transform :: App :~> Handler
  transform = NT (runAppAsHandler env)
