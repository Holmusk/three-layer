module Rename where 

import Data.Text (Text)
import Data.Semigroup ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Tests to copy and paste into stack repl, make sure in src folder
{-
:{
a = changeLine "CHANGE" "import Lib.App (AppEnv (..)) <- TEST 1"
b = changeLine "CHANGE" "module Lib where <- TEST 2"
c = changeLine "CHANGE" "( module Lib ) <- TEST 3"
d = changeLine "CHANGE" ", module Lib <- TEST 4"
e = changeLine "CHANGE" "import Servant.Server (serve) <- TEST 5"
x = [a,b,c,d,e]
:}
putStr (T.unpack (T.unlines x))
-}
changeLine :: Text -> Text -> Text 
changeLine newMod line = case T.words line of 
    -- module names 
    "module" : s : extra     -> T.unwords ("module": prefix s : extra)

    -- imports which start with Lib
    "import" : s : extra     -> if T.take libLen s == "Lib" 
                                then T.unwords ("import": prefix s : extra) 
                                else line 

    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> T.unwords ("(" :"module": prefix s : extra)
    ",":"module" : s : extra -> T.unwords ("," :"module": prefix s : extra)

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
rename newMod s = T.unlines [changeLine newMod x | x <- T.lines s]

-- Method with filePath
contentRename :: (Text -> Text -> Text) -> Text -> FilePath -> IO ()
contentRename f newMod file = f newMod <$> TIO.readFile file >>= TIO.putStrLn