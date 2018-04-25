module Lib.App.Error where

import           Protolude

data AppError =
    Invalid Text
  | NotAllowed Text
  | NotFound
  | ServerError Text
  deriving (Show, Eq)
