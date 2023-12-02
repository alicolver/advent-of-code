module Utils.StringUtils where 

splitStringOn :: (Char -> Bool) -> String -> [String]
splitStringOn p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitStringOn p s''
                            where (w, s'') = break p s'

containsWord :: String -> String -> Bool
containsWord word = elem word . words

replace :: Char -> Char -> Char -> Char
replace toReplace replaceWith char = 
    if toReplace == char 
    then replaceWith 
    else char