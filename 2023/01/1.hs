import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
    input <- readFile "01/input.txt"
    let splitInput = lines input
    print (day1 splitInput)

day1 :: [String] -> Int
day1 input = sum(map getTwoDigitInteger input)

getTwoDigitInteger :: String -> Int
getTwoDigitInteger xs = (10 * getInt xs) + getInt (reverse xs)

getInt :: String -> Int
getInt [] = 0
getInt (x:xs) =
    if isDigit x
    then digitToInt x
    else getInt xs