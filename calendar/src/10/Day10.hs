module Day10 (
    day10
) where
import Lib (mapWithIndices)
import Data.List (nub)

day10 :: IO()
day10 = do
    input <- readFile "src/10/input.txt"
    let intPoints = map toInts (lines input)
    let points = mapWithIndices $ intPoints
    let starts = concatMap (filter (\a -> fst a == 0)) points
    let rcs = (length points, length (head points))
    print (p1 starts points rcs)
    print (p2 starts points rcs)

p1 :: [(Int, (Int,Int))] -> [[(Int, (Int,Int))]] -> (Int,Int) -> Int 
p1 starts matrix rcs = sum $ map (length . nub . getPoints matrix rcs) starts

p2 :: [(Int, (Int,Int))] -> [[(Int, (Int,Int))]] -> (Int,Int) -> Int 
p2 starts matrix rcs = sum $ map (length . getPoints matrix rcs) starts

toInts :: [Char] -> [Int]
toInts = map (\a -> read [a])

getPoints :: [[(Int, (Int,Int))]] -> (Int, Int) -> (Int, (Int,Int)) -> [(Int, (Int,Int))]
getPoints _ _ (9, (x,y)) = [(9, (x,y))]
getPoints matrix rcs (v, (x,y)) = up ++ down ++ left ++ right
    where
        up = if inGrid (x, y+1) rcs && isInc then getPoints matrix rcs np else []
            where
                np = matrix !! (y+1) !! x
                isInc = fst np == v+1
        down = if inGrid (x, y-1) rcs && isInc then getPoints matrix rcs np else []
            where
                np = matrix !! (y-1) !! x
                isInc = fst np == v+1
        left = if inGrid (x-1, y) rcs && isInc then getPoints matrix rcs np else []
            where
                np = matrix !! y !! (x-1)
                isInc = fst np == v+1
        right = if inGrid (x+1, y) rcs && isInc then getPoints matrix rcs np else []
            where
                np = matrix !! y !! (x+1)
                isInc = fst np == v+1

inGrid :: (Int, Int) -> (Int, Int) -> Bool
inGrid (x,y) (rows,cols) = not (x < 0 || y < 0 || x >= cols || y >= rows)
