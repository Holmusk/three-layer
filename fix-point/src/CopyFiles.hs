module CopyFiles where

import Universum

import Control.Exception (throw)
import Data.Text (Text)
import System.Directory (copyFile, createDirectory, doesDirectoryExist,
                         doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO.Error (userError)

import qualified Data.Text as T
import qualified Rename as R

copyAll :: FilePath -> FilePath -> String -> IO ()
copyAll source target newName = do
    unlessM (doesDirectoryExist source) $
        throw (userError "source does not exist")
    whenM (doesFileOrDirectoryExist target) $
        throw (userError "destination already exists")

  -- if bottom two lines swapped, infinite creation of target occurs
    content <- filter norFP3Layer <$> listDirectory source
    createDirectory target
    for_ content $ \name -> do
        let sourcePath = source </> name
        let targetPath = case name of
                          "Lib"    -> target </> newName
                          "Lib.hs" -> target </> (newName ++ ".hs")
                          _        -> target </> name
        isDirectory <- doesDirectoryExist sourcePath
        -- if directory, recurse until a file is reached
        -- Then rename the modules inside a haskell file
        if isDirectory
        then copyAll sourcePath targetPath newName
        else do
          copyFile sourcePath targetPath
          when (takeExtension sourcePath == ".hs") $
              R.contentRename R.rename (toText newName) targetPath
          when (name == "package.yaml") $
              R.contentRename renameYaml (toText newName) targetPath

  where
    doesFileOrDirectoryExist :: FilePath -> IO Bool
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]

    norFP3Layer :: FilePath -> Bool
    norFP3Layer x = not (x == "fix-point" || x == "three-layer.cabal")

renameYaml :: Text -> Text -> Text
renameYaml new s = unlines [T.replace "three-layer" new x | x <- lines s]
