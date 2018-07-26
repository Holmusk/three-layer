module CopyFiles where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.Text (pack)
import System.Directory (copyFile, createDirectory, doesDirectoryExist,
                         doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import qualified Rename as R

copyAll :: FilePath -> FilePath -> String -> IO ()
copyAll source target newName = do
    unlessM (doesDirectoryExist source) $
        throw (userError "source does not exist")
    whenM (doesFileOrDirectoryExist target) $
        throw (userError "destination already exists")

  -- if bottom two lines swapped, infinite creation of target occurs
    content <- filter (/= "fix-point") <$> listDirectory source
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
          if (takeExtension sourcePath == ".hs")
          then R.contentRename R.rename (pack newName) targetPath
          else return ()

  where
    unlessM s r = s >>= flip unless r
    whenM s r = s >>= flip when r

    doesFileOrDirectoryExist :: FilePath -> IO Bool
    doesFileOrDirectoryExist x =
      or <$> sequence [doesDirectoryExist x, doesFileExist x]
