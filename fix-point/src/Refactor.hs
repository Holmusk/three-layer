module Rename where 

import qualified Data.Text as Universum
import qualified Data.Text.IO as Universum

changeLine :: Universum.Text -> Universum.Text -> Universum.Text 
changeLine newMod line = case Universum.words line of 
    -- module names 
    "module" : s : extra     -> Universum.unwords ("module": prefix s : extra)

    -- imports which start with Lib
    "import" : s : extra     -> if Universum.take libLen s == "Lib" 
                                then Universum.unwords ("import": prefix s : extra) 
                                else line 

    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> Universum.unwords ("(" :"module": prefix s : extra)
    ",":"module" : s : extra -> Universum.unwords ("," :"module": prefix s : extra)

    -- Anything else can't be changed
    _ -> line
  where
    prefix s = renamePrefix newMod (Universum.splitAt libLen s)

-- To use and avoid calling length function too many times 
libLen :: Int
libLen = 3

-- Function replaces Lib prefix with given 1st argument 
renamePrefix :: Universum.Text -> (Universum.Text, Universum.Text) -> Universum.Text
renamePrefix newMod (_, end) = Universum.concat [newMod, end] 

-- Method to test if possible without filepath yet 
rename :: Universum.Text -> Universum.Text -> Universum.Text
rename newMod s = Universum.unlines [changeLine newMod x | x <- Universum.lines s]

-- Method with filePath
contentRename :: (Universum.Text -> Universum.Text -> Universum.Text) -> Universum.Text -> FilePath -> IO ()
contentRename f newMod file = f newMod <$> Universum.readFile file >>= Universum.putStrLn