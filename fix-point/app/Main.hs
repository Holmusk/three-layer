module Main where

import Data.Char (toLower, toUpper)
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, metavar,
                            progDesc, strArgument, strOption, (<**>))
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
    -- let prefix = maybe project upperHead pref
    let prefix = fromMaybe (upperHead project) pref
    let sourceDir = fromMaybe "three-layer" source
    copyAll sourceDir project prefix
  where
    upperHead :: String -> String
    upperHead (start:body) = toUpper start : body
    upperHead []           = []

data Options = Options
    { projectName     :: String
    , prefixName      :: Maybe String
    , sourceDirectory :: Maybe String
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> parseProject
    <*> optional parsePref
    <*> optional parseSD

-- Regular options
parseProject :: Parser String
parseProject = strArgument
    (  metavar "PROJECT_NAME"
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
