import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe)

data ComingFrom = B | T | L | R deriving (Enum, Eq)

main :: IO ()
main = do
    input <- readFile "10/input.txt"
    let map = lines input 
    let rps = runMaze map (findStart map 0)
    print (length rps `div` 2)
    print ((abs (sl rps) - length rps + 3) `div` 2)

sl :: [(Int, Int)] -> Int
sl [_] = 0
sl ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x2 - x1) + sl ((x2, y2) : xs)

runMaze :: [[Char]] -> (Int, Int) -> [(Int, Int)] 
runMaze maze (startRow, startCol) = getDirection maze ((startRow, startCol), T) []

getDirection :: [[Char]] -> ((Int, Int), ComingFrom) -> [(Int, Int)] -> [(Int, Int)]
getDirection maze ((row, col), d) ps = if curr == 'S' && length ps /= 0
    then ps 
    else getDirection maze (direction curr ((row, col), d)) (ps ++ [(row, col)])
    where 
        curr = maze !! row !! col

direction :: Char -> ((Int, Int), ComingFrom) -> ((Int, Int), ComingFrom)
direction '|' ((row, col), d) = if d == T then ((row + 1, col), T) else ((row - 1, col), B)
direction '-' ((row, col), d) = if d == L then ((row, col + 1), L) else ((row, col - 1), R)
direction 'L' ((row, col), d) = if d == T then ((row, col + 1), L) else ((row - 1, col), B)
direction 'J' ((row, col), d) = if d == T then ((row, col - 1), R) else ((row - 1, col), B)
direction '7' ((row, col), d) = if d == L then ((row + 1, col), T) else ((row, col - 1), R)
direction 'F' ((row, col), d) = if d == R then ((row + 1, col), T) else ((row, col + 1), L)
direction 'S' ((row, col), d) = ((row, col +1), L)

findStart :: [[Char]] -> Int -> (Int, Int)
findStart (x:xs) row = 
    if elem 'S' x 
        then (row, fromMaybe 0 (elemIndex 'S' x))
        else findStart xs (row + 1)