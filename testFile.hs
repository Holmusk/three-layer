module ParseStuff where 

parseStuff :: String -> String
parseStuff s = case s of 
    ('T':_) -> "This string begins with T, way to go!"
    ('E':_) -> "An E was found at the start, good job"
    ('J':_) -> "Looks like we found a J at position 0, nice one"
    _       -> "Buzzer sound for you, no desired characters!"