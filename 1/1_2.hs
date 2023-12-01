import Data.Char (isDigit, digitToInt)
import Data.List

main :: IO ()
main = do
    input <- readFile "1/input.txt"
    let splitInput = lines input
    print (day1 splitInput)

day1 :: [String] -> Int
day1 input = sum(map getTwoDigitInteger input)

getTwoDigitInteger :: String -> Int
getTwoDigitInteger xs = (10 * getInt(sublists xs)) + getInt(reverse(sublists xs))

getInt :: [String] -> Int
getInt [] = 0
getInt xs = unwrap(find (/=0) (map getInt' xs))

unwrap :: Maybe Int -> Int
unwrap Nothing = 0
unwrap (Just x) = x

getInt' :: String -> Int
getInt' [] = 0
getInt' [x] =
    if isDigit x
    then digitToInt x
    else 0
getInt' xs =
    if getIntFromString xs /= 0
    then getIntFromString xs
    else 0

getIntFromString :: String -> Int
getIntFromString [] = 0
getIntFromString xs =
    if stringToNumber xs /= 0
    then stringToNumber xs
    else 0

sublists :: String -> [String]
sublists = concatMap (tail . inits) . tails

stringToNumber :: String -> Int
stringToNumber "one" = 1
stringToNumber "two" = 2
stringToNumber "three" = 3
stringToNumber "four" = 4
stringToNumber "five" = 5
stringToNumber "six" = 6
stringToNumber "seven" = 7
stringToNumber "eight" = 8
stringToNumber "nine" = 9
stringToNumber x = 0