{-# LANGUAGE KindSignatures #-}

-- | This module introduce aliases to use for @servant-generic@ types and functions writing.

module Lib.Server.Types
       ( AppServer
       , ToApi
       ) where

import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)

import Lib.App (App)

type AppServer = AsServerT App
type ToApi (site :: * -> *) = ToServantApi site
