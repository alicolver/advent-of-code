module Day11(
    day11
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.MemoTrie (memo2)
import Lib (intParser)

solve :: [Int] -> Int -> Int
solve nums iterations = sum $ map (applyRule iterations) nums

applyRule :: Int -> Int -> Int
applyRule = memo2 applyRule'

applyRule' :: Int -> Int -> Int
applyRule' 0 _ = 1
applyRule' i x
    | x == 0 = applyRule (i-1) 1
    | even (length stringRep) = applyRule (i-1) firstHalf + applyRule (i-1) secondHalf
    | otherwise = applyRule (i-1) (x * 2024)
    where
        stringRep = show x
        halfLength = length stringRep `div` 2
        firstHalf = read $ take halfLength stringRep
        secondHalf = read $ drop halfLength stringRep

-- parsing and orchestration 

intlist :: Parser [Int]
intlist = intParser `sepBy` space

parseIntList :: String -> [Int]
parseIntList input = case parse intlist "" input of
    Left err -> error (show err)
    Right result -> result

day11 :: IO()
day11 = do
    input <- readFile "src/input/11.txt"
    let nums = parseIntList input
    print (solve nums 25)
    print (solve nums 75)