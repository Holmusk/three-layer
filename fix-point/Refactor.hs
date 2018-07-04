module Refactor where 
import Data.List 

changeLine :: String -> String -> String 
changeLine newMod line = case words line of 
    -- module names 
    "module" : s : extra -> 
        unwords ("module": (transform newMod $ splitAt 3 s) : extra)

    -- imports which start with Lib
    "import" : s : extra -> 
        if take 3 s == "Lib" then 
        unwords ("import": (transform newMod $ splitAt 3 s) : extra) 
        else 
            line 

    -- reexport where reexport module started with Lib
    "(":"module" : s : extra -> 
        unwords ("(" :"module": (transform newMod $ splitAt 3 s) : extra)
    
    -- Anything else can't be changed 
    _ -> line

transform :: String -> (String, String) -> String
transform s (_,end) = s++end 

-- Method to test I could do it without filepath yet 
refactor :: String -> String -> String
refactor newMod s = unlines [changeLine newMod x | x <- lines s]

-- Method with filePath
contentRefactor :: (String -> String -> String) -> String -> FilePath -> IO ()
contentRefactor f newMod file = f newMod <$> readFile file >>= putStrLn

  

