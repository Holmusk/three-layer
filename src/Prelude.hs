{-# OPTIONS -fno-warn-orphans #-}

-- | Uses @universum as default prelude

module Prelude
       ( module Universum
       , UUID
       , textToLBS
       ) where

import Universum

import Data.UUID.Types (UUID)
import Elm (ElmType (..))

import qualified Data.ByteString.Lazy as BSL (fromStrict)
import qualified Data.UUID.Types as UUID

-- | Converts 'Text' to 'LByteString'.
textToLBS :: Text -> LByteString
textToLBS = BSL.fromStrict . encodeUtf8

instance ElmType UUID where
  toElmType = toElmType . UUID.toString
