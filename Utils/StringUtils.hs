module Utils.StringUtils where
import Data.Char
import Text.Parsec.String
import Text.Parsec

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

nonDigit :: Char -> Bool
nonDigit = not . isDigit

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers xs = do
    let afterDroppingNonDigits = dropWhile nonDigit xs
    let num = takeWhile isDigit afterDroppingNonDigits
    let afterDroppingNumber = dropWhile isDigit afterDroppingNonDigits
    if null num then []
    else (read num :: Int) : extractNumbers afterDroppingNumber

integer :: Parser Int
integer = read <$> many1 digit