module Day16 (
    day16
) where
import Lib (mapWithIndices)

day16 :: IO ()
day16 = do
    input <- readFile "src/input/16.txt"
    let grid = mapWithIndices $ lines input
    print input
