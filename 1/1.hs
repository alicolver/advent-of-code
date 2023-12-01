import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
    input <- readFile "1/input.txt"
    let splitInput = lines input
    print (day1 splitInput)

day1 :: [String] -> Int
day1 input = sum(map getTwoDigitInteger input)

getTwoDigitInteger :: String -> Int
getTwoDigitInteger xs = (10 * getLeftInt xs) + getRightInt xs

getLeftInt :: String -> Int
getLeftInt [] = 0
getLeftInt (x:xs) =
    if isDigit x
    then digitToInt x
    else getLeftInt xs

getRightInt :: String -> Int
getRightInt xs = getLeftInt(reverse xs)