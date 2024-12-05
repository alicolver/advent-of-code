module Lib
    ( numMatches,
    doesMatch,
    getSlidingWindow2d,
    getDiagonals
    ) where

import Text.Regex.Posix

numMatches :: String -> String -> Int
numMatches reg i = length (findMatches reg i)

findMatches :: String -> String -> [String]
findMatches pattern input = getAllTextMatches (input =~ pattern) :: [String]

doesMatch :: String -> String -> Bool
doesMatch pattern input = input =~ pattern

getSlidingWindow2d :: Int -> [[a]] -> [[[a]]]
getSlidingWindow2d window matrix
    | length matrix < window || any (< window) (map length matrix) = []
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