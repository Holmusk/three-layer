module Rename where

import Universum

import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Text as T

-- Tests to copy and paste into stack repl, make sure in src folder
{-
import qualified Data.Text.IO as TIO
:{
TIO.putStrLn $ rename "CHANGE" "import Lib.App (AppEnv (..))\n\
\module Lib where\n\
\( module Lib )\n\
\, module Lib\n\
\import Servant.Server (serve)"
:}
-}
changeLine :: Text -> Text -> Text
changeLine newMod line = decide line
  where
    prefix s = renamePrefix newMod (T.splitAt libLen s)

    decide :: Text -> Text
    decide l = case words l of
        ("(" : "module" : str : ex) -> if take libLen (toString str) == "Lib"
                                       then unwords ("(" : "module" : prefix str : ex)
                                       else l
        ("," : "module" : str : ex) -> if take libLen (toString str) == "Lib"
                                       then unwords ("," : "module" : prefix str : ex)
                                       else l
        ("import" : str : ex)       -> if take libLen (toString str) == "Lib"
                                       then unwords ("import" : prefix str : ex)
                                       else l
        ("module" : str : ex)       -> if take libLen (toString str) == "Lib"
                                       then unwords ("module" : prefix str : ex)
                                       else l
        _ -> l


-- To use and avoid calling length function too many times
libLen :: Int
libLen = 3

-- Function replaces Lib prefix with given 1st argument
renamePrefix :: Text -> (Text, Text) -> Text
renamePrefix newMod (_, end) = newMod <> end

-- Method to test if possible without filepath yet
rename :: Text -> Text -> Text
rename newMod s = unlines [changeLine newMod x | x <- lines s]

-- Method with filePath
contentRename :: (Text -> Text -> Text) -> Text -> FilePath -> IO ()
contentRename f newMod file = do
    content <- f newMod <$> readFile file
    writeFile file content
