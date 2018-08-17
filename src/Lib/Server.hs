{-# LANGUAGE TypeOperators #-}

module Lib.Server
       ( API
       , server
       ) where

import Servant.API.Generic (toServant)
import Servant.Server (Server, hoistServer)

import Lib.App (AppEnv, runAppAsHandler)
import Lib.Server.Auth (AuthAPI, authServer)

type API = AuthAPI

server :: AppEnv -> Server API
server env = hoistServer (Proxy @API) (runAppAsHandler env) (toServant authServer)
