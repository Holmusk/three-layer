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
changeLine newMod line = case words line of
    -- module names
    "module" : s : extra     -> unwords ("module": prefix s : extra)

    -- imports which start with Lib
    "import" : s : extra     -> if take libLen (T.unpack s) == "Lib"
                                then unwords ("import": prefix s : extra)
                                else line

    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> unwords ("(" :"module": prefix s : extra)
    ",":"module" : s : extra -> unwords ("," :"module": prefix s : extra)

    -- Anything else can't be changed
    _ -> line
  where
    prefix s = renamePrefix newMod (T.splitAt libLen s)

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
