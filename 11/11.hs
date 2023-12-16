import Data.List

main :: IO ()
main = do 
    input <- readFile "11/input.txt"
    let m = lines input
    let expanded = transpose (expand (transpose (expand m)))
    let galaxyCoords = findGalaxies '#' expanded
    print (calcPathLengths galaxyCoords)
    let rowIndices = map snd (expandIntoCoord m 0)
    let colIndices = map snd (expandIntoCoord (transpose m) 0)
    let coordinates = addCoordinates m rowIndices colIndices 
    print (coordinates)
    let p2Cs = fg2 coordinates
    print (calcPathLengths (p2Cs))

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

expand :: [[Char]] -> [[Char]]
expand = concatMap (\x -> if all (=='.') x then [x, x] else [x])

expandIntoCoord :: [[Char]] -> Int -> [([Char], Int)]
expandIntoCoord [] cnt = []
expandIntoCoord (r:rs) cnt = (r, newCnt) : expandIntoCoord rs newCnt 
    where 
        newCnt = if all (=='.') r then cnt + (1_000_000) else cnt + 1

findGalaxies :: Char -> [[Char]] -> [(Int, Int)]
findGalaxies char grid = [ (row, col)
  | (row, rowChars) <- zip [0..] grid
  , (col, gridChar) <- zip [0..] rowChars
  , gridChar == char
  ]

fg2 :: [(Char, (Int, Int))] -> [(Int, Int)]
fg2 [] = []
fg2 ((c, (row, col)):cs) = if c == '#' then (row, col) : fg2 cs else fg2 cs 