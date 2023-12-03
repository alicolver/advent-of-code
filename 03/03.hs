import Utils.StringUtils
import Utils.Utils
import Data.Char (isDigit, digitToInt)

data Number = Number {
    value :: Int,
    row :: Int,
    start :: Int,
    end :: Int
} deriving (Eq, Ord, Show)

data Symbol = Symbol {
    symbol :: Char,
    srow :: Int,
    col :: Int
} deriving (Show)

main :: IO ()
main = do
    input <- readFile "03/input.txt"
    let splitInput = lines input
    let numbers = getNumbers (zip [0..] splitInput)
    let symbols = getSymbols (zip [0..] splitInput)
    let numbersNearSymbol = filter (isNumberNearSymbol symbols) numbers
    print (sum (map getValue numbersNearSymbol))
    let gears = getGears (zip [0..] splitInput)
    print (sum (map (getGearValue numbers) gears))

getGearValue :: [Number] -> Symbol -> Int
getGearValue nums gear = if length filteredNums == 2 then product (map value filteredNums) else 0
    where filteredNums = filter (isNumberNearSymbol [gear]) nums

isNumberNearSymbol :: [Symbol] -> Number -> Bool
isNumberNearSymbol symbols number
  = foldr
      (\ symbol
         -> (||)
              (inRowRange (srow symbol) (row number)
                 && inColRange (col symbol) number))
      False symbols

inColRange :: Int -> Number -> Bool
inColRange symbolCol number = symbolCol >= (start number - 1) && symbolCol <= (end number + 1)

inRowRange :: Int -> Int -> Bool
inRowRange symbolRow numberRow = (symbolRow <= numberRow + 1) && (symbolRow >= numberRow - 1)

getValue :: Number -> Int
getValue = value

getNumbers :: [(Int, String)] -> [Number]
getNumbers = flatmap getNumbers'

getNumbers' :: (Int, String) -> [Number]
getNumbers' (rowNumber, row) = getNumbers'' (rowNumber, zip [0..] row)

getNumbers'' :: (Int, [(Int, Char)]) -> [Number]
getNumbers'' (_, []) = []
getNumbers'' (row, (col,char):colChars) =
    if isDigit char
    then Number (calculate (map snd colChars) (digitToInt char)) row col (col+endVal)
        : getNumbers'' (row, drop endVal colChars)
    else getNumbers'' (row, colChars)
    where endVal = calculateEnd (map snd colChars) 0

calculate :: [Char] -> Int -> Int
calculate [] count = count
calculate (x:xs) count =
    if isDigit x
    then calculate xs ((count * 10) + digitToInt x )
    else count

calculateEnd :: [Char] -> Int -> Int
calculateEnd [] y = y
calculateEnd (x:xs) y = if isDigit x then calculateEnd xs (y+1) else y

getSymbols :: [(Int, String)] -> [Symbol]
getSymbols = flatmap (getSymbols' isSymbol)

getGears :: [(Int, String)] -> [Symbol]
getGears = flatmap (getSymbols' isGear)

getSymbols' :: (Char -> Bool) -> (Int, String) -> [Symbol]
getSymbols' checkChar (rowNumber, row) = getSymbols'' (rowNumber, zip [0..] row) checkChar

getSymbols'' :: (Int, [(Int, Char)]) -> (Char -> Bool) -> [Symbol]
getSymbols'' (_, []) _ = []
getSymbols'' (rowNumber, (col,char):colChars) checkChar = if checkChar char
    then Symbol char rowNumber col : getSymbols'' (rowNumber,colChars) checkChar
    else getSymbols'' (rowNumber, colChars) checkChar

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'

isGear :: Char -> Bool
isGear x = x == '*'