module Rename where 

changeLine :: String -> String -> String 
changeLine newMod line = case words line of 
    -- module names 
    "module" : s : extra     -> unwords ("module": r1 : extra)
    -- imports which start with Lib
    "import" : s : extra     -> if take (length "Lib") s == "Lib" 
                                then unwords ("import": r1 : extra) 
                                else line 
    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> unwords ("(" :"module": r2 : extra)
    ",":"module" : s : extra -> unwords ("," :"module": r2 : extra)
    -- Anything else can't be changed
    _ -> line
  where
    r1 = renamePrefix newMod $ splitAt (length "Lib") ((words line)!!1)
    r2 = renamePrefix newMod $ splitAt (length "Lib") ((words line)!!2)
-- Function replaces Lib prefix with given 1st argument 
renamePrefix :: String -> (String, String) -> String
renamePrefix s ( _ ,end) = s ++ end 

-- Method to test I could do it without filepath yet 
rename :: String -> String -> String
rename newMod s = unlines [changeLine newMod x | x <- lines s]

-- Method with filePath
contentRename :: (String -> String -> String) -> String -> FilePath -> IO ()
contentRename f newMod file = f newMod <$> readFile file >>= putStrLn