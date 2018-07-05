module Rename where 

changeLine :: String -> String -> String 
changeLine newMod line = case words line of 
    -- module names 
    "module" : s : extra     -> unwords ("module": newPref newMod s : extra)

    -- imports which start with Lib
    "import" : s : extra     -> if take libLen s == "Lib" 
                                then unwords ("import": newPref newMod s : extra) 
                                else line 

    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> unwords ("(" :"module": newPref newMod s : extra)
    ",":"module" : s : extra -> unwords ("," :"module": newPref newMod s : extra)

    -- Anything else can't be changed
    _ -> line

-- To use and avoid calling length function too many times 
libLen :: Int
libLen = length "Lib"

-- Function replaces Lib prefix with given 1st argument 
renamePrefix :: String -> (String, String) -> String
renamePrefix newMod (_, end) = newMod ++ end 

-- higher level rename 
newPref :: String -> String -> String
newPref newMod s = renamePrefix newMod (splitAt libLen s)

-- Method to test if possible without filepath yet 
rename :: String -> String -> String
rename newMod s = unlines [changeLine newMod x | x <- lines s]

-- Method with filePath
contentRename :: (String -> String -> String) -> String -> FilePath -> IO ()
contentRename f newMod file = f newMod <$> readFile file >>= putStrLn