module Day04 (
    day4
) where

import Data.List ( transpose )
import Lib
import Data.Universe.Helpers

day4 :: IO ()
day4 = do
    input <- readFile "src/input/04.txt"
    let splitup = lines input
    print (day4' (getAllArrays splitup))
    print (day4_2 splitup)

day4' :: [[Char]] -> Int
day4' chars = sum (map (numMatches "XMAS") chars)

day4_2 :: [[Char]] -> Int
day4_2 chars = length (filter isValidDiag (map getDiagonals (getSlidingWindow2d 3 chars)))

getAllArrays :: [[Char]] -> [[Char]]
getAllArrays x = x ++
                map reverse x ++
                transpose x ++
                transpose (reverse x) ++
                diagonals x  ++
                diagonals (map reverse x) ++
                diagonals (transpose x) ++
                diagonals (transpose (map reverse x))

isValidDiag :: ([Char], [Char]) -> Bool
isValidDiag (a,b) =  doesMatch "MAS|SAM" a && doesMatch "MAS|SAM" b