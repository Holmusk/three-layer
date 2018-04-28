{-# LANGUAGE TypeOperators #-}
module Lib.Server where

import           Lib.App
import           Lib.Server.Auth
import           Lib.Server.Search
import           Servant
import           Servant.Server

type API = AuthAPI :<|> SearchAPI

server :: AppEnv -> Server API
server env = enter transform combinedServers
  where
  combinedServers = authServer :<|> searchServer
  transform :: App :~> Handler
  transform = NT (runAppAsHandler env)
