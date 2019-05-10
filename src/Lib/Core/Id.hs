{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Contains safe 'Id' representation.

module Lib.Core.Id
       ( -- * Id
         Id (..)
       , AnyId
       , castId
       ) where

import Data.Type.Equality (type (==))
import Elm.Generic (elmNewtype)


-- | Wrapper for textual id. Contains phantom type parameter for increased
-- type-safety.
newtype Id a = Id { unId :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromHttpApiData, FromJSON, ToJSON)

instance Elm (Id a) where
    toElmDefinition _ = elmNewtype @Text "Id" "unId"

-- | When we don't care about type of 'Id' but don't want to deal with type variables
type AnyId = Id ()

-- | Unsafe cast of 'Id'. Implementation uses smart trick to enforce usage
-- always with @TypeApplications@.
castId ::forall to from to' . ((to == to') ~ 'True) => Id from -> Id to'
castId (Id a) = Id a
