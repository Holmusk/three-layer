module CopyFiles where

import Control.Applicative ((<$>))
import Control.Exception (throw)
import Control.Monad (when,forM_)
import Data.Functor ((<$>))
import System.Directory
import System.FilePath ((</>))
-- import System.File.Tree (getDirectory, copyTo_)

import qualified Rename as R

-- getDirectory :: FilePath -> IO System.File.Tree.FSTree
-- copyTo_ :: FilePath -> System.File.Tree.FSTree -> IO ()
-- copyDir :: FilePath -> FilePath -> IO ()
-- copyDir source target = getDirectory source >>= copyTo_ target

copyAll :: FilePath -> FilePath -> IO ()
copyAll source target = do
  -- when :: Applicative f => Bool -> f () -> f ()
  when' (not <$> doesDirectoryExist source) $
    throw (userError "source does not exist")
  when' (doesFileOrDirectoryExist target) $
    throw (userError "destination already exists")

  -- if bottom two lines swapped, infinite creation of target occurs
  content <- listDirectory source
  createDirectory target
  let xs = content
  forM_ xs $ \name -> do
    -- (</>) :: FilePath -> FilePath -> FilePath, combines two paths with
    -- path separator
    let sourcePath = source </> name
    let targetPath = target </> name
    isDirectory <- doesDirectoryExist sourcePath
    -- if directory, recurse until a file is reached
    if isDirectory
      then copyAll sourcePath targetPath
      else copyFile sourcePath targetPath

  where
    -- IO [Bool] -> IO Bool
    doesFileOrDirectoryExist x =
      or <$> sequence [doesDirectoryExist x, doesFileExist x]
    when' s r = s >>= flip when r
