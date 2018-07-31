module MoveUtil where

import Universum

import Control.Exception (throw)
import Data.Text (Text)
import System.Directory (copyFile, createDirectory, doesDirectoryExist,
                         doesFileExist, removeDirectory, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Error (userError)

moveUtil :: FilePath -> IO ()
moveUtil tgt = do
  copyDir "Util" tgt
  removeDirectory "Util"

copyDir :: FilePath -> FilePath -> IO ()
copyDir source target = do
    unlessM (doesDirectoryExist source) $
        throw (userError "source does not exist")
    whenM (doesFileOrDirectoryExist target) $
        throw (userError "destination already exists")

    content <- listDirectory source
    createDirectory target
    for_ content $ \name -> do
        let sourcePath = source </> name
        let targetPath = target </> name
        isDirectory <- doesDirectoryExist sourcePath
        if isDirectory
        then copyAll sourcePath targetPath newName
        else copyFile sourcePath targetPath
  where
    doesFileOrDirectoryExist :: FilePath -> IO Bool
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
