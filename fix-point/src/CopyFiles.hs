module CopyFiles where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (forM_
                    , when)
import System.Directory (copyFile
                       , createDirectory
                       , doesDirectoryExist
                       , doesFileExist
                       , listDirectory)
import System.FilePath ((</>))

import qualified Rename as R

copyAll :: FilePath -> FilePath -> IO ()
copyAll source target = do
    whenM (not <$> doesDirectoryExist source) $
        throw (userError "source does not exist")
    whenM (doesFileOrDirectoryExist target) $
        throw (userError "destination already exists")

  -- if bottom two lines swapped, infinite creation of target occurs
    content <- listDirectory source
    createDirectory target
    forM_ content $ \name -> do
        let sourcePath = source </> name
        let targetPath = target </> name
        isDirectory <- doesDirectoryExist sourcePath
        -- if directory, recurse until a file is reached
        if isDirectory
        then copyAll sourcePath targetPath
        else copyFile sourcePath targetPath

    where
        whenM s r = s >>= flip when r
        doesFileOrDirectoryExist :: FilePath -> IO Bool
        doesFileOrDirectoryExist x =
          or <$> sequence [doesDirectoryExist x, doesFileExist x]
