module Rename where

import Data.Text (Text)
import Data.Semigroup ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
-- Method to test if possible without filepath yet
rename :: Text -> Text -> Text
rename newMod s = T.unlines [T.replace "Lib" newMod x | x <- T.lines s]

-- Method with filePath
contentRename :: (Text -> Text -> Text) -> Text -> FilePath -> IO ()
contentRename f newMod file = do
    content <- f newMod <$> TIO.readFile file
    TIO.writeFile file content
