module Lib
    ( numMatches,
    doesMatch,
    slidingWindow,
    getSlidingWindow2d,
    getDiagonals,
    mapWithIndices,
    intParser
    ) where
import Text.Parsec
import Text.Parsec.String
import Text.Regex.Posix
import Data.Maybe (isNothing)

intParser :: Parser Int
intParser = do
    sign <- optionMaybe (char '-')
    val <- many1 digit
    return (if isNothing sign then read val else - (read val))

mapWithIndices :: [[a]] -> [[(a, (Int, Int))]]
mapWithIndices matrix =
    [ [ (i, (c, r)) | (c, i) <- zip [0..] row ] | (r, row) <- zip [0..] (reverse matrix) ]

numMatches :: String -> String -> Int
numMatches reg i = length (findMatches reg i)

findMatches :: String -> String -> [String]
findMatches pattern input = getAllTextMatches (input =~ pattern) :: [String]

doesMatch :: String -> String -> Bool
doesMatch pattern input = input =~ pattern

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs
    | n > length xs = []
    | otherwise = take n xs : slidingWindow n (tail xs)

getSlidingWindow2d :: Int -> [[a]] -> [[[a]]]
getSlidingWindow2d window matrix
    | length matrix < window || any ((< window) . length) matrix = []
    | otherwise = [ getSubMatrix i j | i <- [0 .. numRows - window], j <- [0 .. numCols - window] ]
    where
        numRows = length matrix
        numCols = length (head matrix)
        getSubMatrix x y = [take window (drop y row) | row <- take window (drop x matrix)]

getDiagonals :: [[a]] -> ([a], [a])
getDiagonals matrix = (main, second)
    where
        numRows = length matrix
        numCols = length (head matrix)
        main = [matrix !! i !! i | i <- [0 .. min numRows numCols - 1]]
        second = [matrix !! i !! (numCols - 1 - i) | i <- [0 .. min numRows numCols - 1]]