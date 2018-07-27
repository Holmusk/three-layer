module Main where

import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)

import CopyFiles (copyAll)
import Rename

main :: IO ()
main = do
    args <- getArgs
    isDirectory <- doesDirectoryExist "three-layer"
    if isDirectory
    then copyAll "three-layer" args!!0 args!!1
    else putStrLn "three-layer directory does not exist"
