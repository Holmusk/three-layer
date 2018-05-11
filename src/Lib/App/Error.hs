module Lib.App.Error
       ( AppError (..)
       ) where

data AppError =
    Invalid Text
  | NotAllowed Text
  | NotFound
  | ServerError Text
  deriving (Show, Eq)
