module Main where

import Control.Applicative (optional)
import Data.Semigroup ((<>))
import Options.Applicative (Parser, long, metavar, help, helper, progDesc,
                            fullDesc, header, info, (<**>), execParser, strOption)
import System.Directory (doesDirectoryExist, getCurrentDirectory)

import CopyFiles (copyAll)
import Rename

main :: IO ()
main = test =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      (  fullDesc
      <> progDesc "Test parsing of CLI's"
      <> header "Test for optparse-applicative")

test :: Options -> IO ()
test opts = case opts of
    (Options proj (Just pref) (Just sd)) -> copyAll sd proj pref
    (Options proj Nothing (Just sd))     -> copyAll sd proj proj
    (Options proj (Just pref) Nothing)   -> copyAll "three-layer" proj pref
    (Options proj Nothing Nothing)       -> copyAll "three-layer" proj proj

data Options = Options
    { projectName :: String
    , prefixName :: Maybe String
    , sourceDirectory :: Maybe String
    }

parseOptions :: Parser Options
parseOptions = Options <$> parseProject <*> parsePref <*> parseSD

-- Regular options
parseProject :: Parser String
parseProject = strOption
    (  long "project-title"
    <> metavar "PROJECTNAME"
    <> help "Name of project")

parseOnlyPref :: Parser String
parseOnlyPref = strOption
    (  long "prefix-name"
    <> metavar "PREFIXNAME"
    <> help "Rename all Lib files and modules to this name")

parsePref :: Parser (Maybe String)
parsePref = optional parseOnlyPref

parseOnlySD :: Parser String
parseOnlySD = strOption
    (  long "source-directory"
    <> metavar "SOURCEPATH"
    <> help "Source path must point to three-layer")

parseSD :: Parser (Maybe String)
parseSD = optional parseOnlySD
