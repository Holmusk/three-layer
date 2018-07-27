module Main where

import Data.Semigroup ((<>))
import Options.Applicative (help, Parser, ParserInfo, info, (<**>), header,
                            helper, execParser, (<|>), metavar, long, short,
                            fullDesc, progDesc, argument, str)
import System.Directory (doesDirectoryExist)

import CopyFiles (copyAll)
import Rename

main :: IO ()
main = run =<< execParser (args `withInfo` "Create project with given name")

run :: Args -> IO ()
run (Args input) = do
    case input of
        Project  proj           -> copyAll "three-layer" proj proj
        ProjPref proj pref      -> copyAll "three-layer" proj pref
        StringFP path           -> copyAll path "NEWNAME" "NEWDIR"
        Combo    path proj pref -> copyAll path proj pref

type Project    = String
type Prefix     = String
type SourcePath = FilePath

data FPInput = Project Project
             | ProjPref Project Prefix
             | StringFP SourcePath
             | Combo SourcePath Project Prefix

data Args = Args FPInput

args :: Parser Args
args = Args <$> parseFPInput

parseFPInput :: Parser FPInput
parseFPInput = project <|> projPref <|> stringFP <|> combo

project :: Parser FPInput
project = Project
    <$> argument str (metavar "PROJECT NAME" <> help "name of project" )

projPref :: Parser FPInput
projPref = ProjPref
    <$> argument str
        (metavar "PROJECT NAME" <> help "name of project" )
    <*> argument str
        (  long "prefix"
        <> short 'p'
        <> metavar "PREFIX NAME"
        <> help "name of prefix" )

stringFP :: Parser FPInput
stringFP = StringFP <$> argument str
           (  long "source path"
           <> short 's'
           <> metavar "SOURCEPATH"
           <> help "source path" )

combo :: Parser FPInput
combo = Combo
    <$> argument str
    (  long "source path"  -- This line gave a 'no instance error'... 
    <> short 's'
    <> metavar "SOURCEPATH"
    <> help "source path" )
    <*> argument str
        (metavar "PROJECT NAME" <> help "name of project" )
    <*> argument str
        (  long "prefix"
        <> short 'p'
        <> metavar "PREFIX NAME"
        <> help "name of prefix" )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
