module Main where

import           Data.Default
import           Lens.Micro
import           Lib
import qualified Proto.Foo    as P
import           Protolude    hiding ((&))

main :: IO ()
main = do
  env <- mkAppEnv
  runServer env

newSearchRequest :: P.SearchRequest
newSearchRequest =
  def & P.query .~ "Hello"
      & P.pageNumber .~ 10
      & P.resultPerPage .~ 20
