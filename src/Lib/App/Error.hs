module Lib.App.Error where

data AppError =
    Invalid Text
  | NotAllowed Text
  | NotFound
  | ServerError Text
  deriving (Show, Eq)
