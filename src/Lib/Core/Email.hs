-- | Newtype wrapper around Email address.

module Lib.Core.Email
       ( Email (..)
       ) where


-- | Newtype for email address.
newtype Email = Email
    { unEmail :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable, FromField, ToField, FromJSON, ToJSON, Elm)
