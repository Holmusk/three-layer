module Lib.Server
       ( Api
       , application
       ) where

import Servant.API.Generic (toServant)
import Servant.Server (Application, Server, hoistServer, serve)

import Lib.App (AppEnv)
import Lib.Effects.Log (runAppAsHandler)
import Lib.Server.Auth (AuthApi, authServer)


type Api = AuthApi

server :: AppEnv -> Server Api
server env = hoistServer
    (Proxy @Api)
    (runAppAsHandler env)
    (toServant authServer)

application :: AppEnv -> Application
application env = serve
    (Proxy @Api)
    (server env)
