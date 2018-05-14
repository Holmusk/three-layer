{-# LANGUAGE TypeOperators #-}

module Lib.Server
       ( API
       , server
       ) where

import Servant.Generic (toServant)
import Servant.Server (Handler, Server, hoistServer)

import Lib.App (App, AppEnv, runAppAsHandler)
import Lib.Server.Auth (AuthAPI, authServer)

type API = AuthAPI

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAppAsHandler env) (toServant authServer)
