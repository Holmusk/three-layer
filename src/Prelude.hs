{-# OPTIONS -fno-warn-orphans #-}

-- | Uses @relude@ as default prelude.

module Prelude
       ( module Relude
       , module Control.Lens
       , UUID
       ) where

-- Reexport
import Relude

import Control.Lens ((.~), (^.))
import Data.UUID.Types (UUID)

-- Internal
import Elm (ElmType (..))

import qualified Data.UUID.Types as UUID

instance ElmType UUID where
    toElmType = toElmType . UUID.toString
