module Main where

import           Lib
import           Protolude

main :: IO ()
main = mkAppEnv >>= runServer
