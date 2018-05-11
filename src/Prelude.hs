-- | Uses @universum as default prelude

module Prelude
       ( module Universum
       , textToLBS
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL (fromStrict)

-- | Converts 'Text' to 'LByteString'.
textToLBS :: Text -> LByteString
textToLBS = BSL.fromStrict . encodeUtf8
