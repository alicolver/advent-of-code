import Data.List

main :: IO ()
main = do 
    input <- readFile "11/input.txt"
    let m = lines input
    print (calc m 1)
    print (calc m 1_000_000)

calc :: [[Char]] -> Int -> Int 
calc m v = calcPathLengths (fg coordinates)
    where 
        rowIndices = map snd (expandIntoCoord m 0 v)
        colIndices = map snd (expandIntoCoord (transpose m) 0 v)
        coordinates = addCoordinates m rowIndices colIndices 

addCoordinates :: [[Char]] -> [Int] -> [Int] -> [(Char, (Int, Int))]
addCoordinates [] [] _ = []
addCoordinates (r:rs) (rowIndex:ris) cs =  addCoordinates' r rowIndex cs ++ addCoordinates rs ris cs

addCoordinates' :: [Char] -> Int -> [Int] -> [(Char, (Int, Int))]
addCoordinates' [] _ [] = []
addCoordinates' (c:cs) rowIndex (cIndex:cIndices) = (c, (rowIndex, cIndex)) : addCoordinates' cs rowIndex cIndices

calcPathLengths :: [(Int, Int)] -> Int 
calcPathLengths (x:[]) = 0
calcPathLengths (x:xs) = calcPathLengths' x xs + calcPathLengths xs

calcPathLengths' :: (Int, Int) -> [(Int, Int)] -> Int 
calcPathLengths' _ [] = 0
calcPathLengths' (r1, c1) ((r2,c2):gs) = abs (r1 - r2) + abs (c1 - c2) + calcPathLengths' (r1, c1) gs

expandIntoCoord :: [[Char]] -> Int -> Int -> [([Char], Int)]
expandIntoCoord [] cnt _ = []
expandIntoCoord (r:rs) cnt v = (r, newCnt) : expandIntoCoord rs newCnt v
    where 
        newCnt = if all (=='.') r then cnt + v else cnt + 1

fg :: [(Char, (Int, Int))] -> [(Int, Int)]
fg [] = []
fg ((c, (row, col)):cs) = if c == '#' then (row, col) : fg cs else fg cs 