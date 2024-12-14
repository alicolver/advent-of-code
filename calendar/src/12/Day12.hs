module Day12(
    day12
) where
import Lib (mapWithIndices)

type PlantLoc = (Char, (Int,Int))

day12 :: IO()
day12 = do
    input <- readFile "src/12/test.txt"
    let farm = mapWithIndices $ lines input
    print farm

getCorners :: [[PlantLoc]] -> PlantLoc -> [[PlantLoc]]
getCorners farm visited = [[]]