{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE PatternSynonyms #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Colog
       , module Control.Lens
       , module Proto

       , WithLog
       , UUID
       ) where

-- Reexport
import Relude

import Control.Lens ((.~), (^.))
import Data.UUID.Types (UUID)

import Colog (pattern D, pattern E, pattern I, LogAction (..), Severity (..), pattern W, log)

import Data.ProtoLens.Message as Proto (defMessage)

-- Internal
import Elm (ElmType (..))

import qualified Data.UUID.Types as UUID
import qualified Colog (Message, WithLog)


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m

instance ElmType UUID where
    toElmType = toElmType . UUID.toString
