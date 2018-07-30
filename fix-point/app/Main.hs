module Main where

import Control.Applicative (optional)
import Data.Char (toUpper, toLower)
import Data.Semigroup ((<>))
import Options.Applicative (Parser, long, metavar, help, helper, progDesc,
                            fullDesc, header, info, (<**>), execParser, strOption)
import System.Directory (doesDirectoryExist, getCurrentDirectory)

import CopyFiles (copyAll)
import Rename

main :: IO ()
main = bootstrap =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      (  fullDesc
      <> progDesc "Refactors with project, prefix and sourcepath with three-layer"
      <> header "Content refactoring")

bootstrap :: Options -> IO ()
bootstrap (Options project pref source) = do
    let prefix = case pref of
                     (Just s) -> s
                     Nothing  -> f project
    let sourceDir = case source of
                        (Just s) -> s
                        Nothing  -> "three-layer"
    copyAll project prefix sourceDir
  where
    f :: String -> String
    f (start:body) = toUpper start : map toLower body
    f [] = []

data Options = Options
    { projectName :: String
    , prefixName :: Maybe String
    , sourceDirectory :: Maybe String
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> parseProject
    <*> optional parsePref
    <*> optional parseSD

-- Regular options
parseProject :: Parser String
parseProject = strOption
    (  long "project-title"
    <> metavar "PROJECT_NAME"
    <> help "Name of project")

parsePref :: Parser String
parsePref = strOption
    (  long "prefix-name"
    <> metavar "PREFIX_NAME"
    <> help "Rename all Lib files and modules to this name")

parseSD :: Parser String
parseSD = strOption
    (  long "source-directory"
    <> metavar "SOURCE_PATH"
    <> help "Source path must point to three-layer")
