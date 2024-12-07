module Day6 (
    day6
) where

import Data.List ( find, minimumBy )
import Data.Ord (comparing)
import Debug.Trace

data Dir = U | L | D | R
    deriving (Eq, Show)

day6 :: IO ()
day6 = do
    rawMap <- readFile "src/06/test.txt"
    let grid = lines rawMap
    let gridWithIndex = mapWithIndices grid
    let start = findStart gridWithIndex
    let rcs = (length grid, length (head grid))
    print rcs
    let pointsOfInterest = map (filter isInterestingChar) gridWithIndex
    print pointsOfInterest
    print (part1 pointsOfInterest start U rcs)

part1 :: [[(Char, (Int, Int))]] -> (Char, (Int, Int)) -> Dir -> (Int, Int) -> Int
part1 pois curPos dir rcs = trace (show curPos ++ show dir ++ show rcs ++ show isInGrid ++ show nextPos) (dist curPos dir nextPos + (if isInGrid then part1 pois nextPos (rotate dir) rcs else 0))
    where
        nextPos = findNextPos curPos dir (concat pois) rcs
        isInGrid = inGrid rcs nextPos

inGrid :: (Int, Int) -> (Char, (Int, Int)) -> Bool
inGrid (rs, cs) (_, (x, y)) = not (x < 0 || y < 0 || y >= rs || x >= cs)

findNextPos :: (Char, (Int, Int)) -> Dir -> [(Char, (Int, Int))] -> (Int, Int) -> (Char, (Int, Int))
findNextPos pos dir potentials (rows, cols) = getAdj (minimumBy (comparing (dist pos dir)) toUse) dir
    where
        inline = filter (isInline dir pos) potentials
        toUse = if null inline then getExitGrid (rows, cols) dir else inline

getAdj :: (Char, (Int, Int)) -> Dir -> (Char, (Int, Int))
getAdj (_, (x, y)) L = ('.', (x + 1, y))
getAdj (_, (x, y)) R = ('.', (x - 1, y))
getAdj (_, (x, y)) U = ('.', (x, y - 1))
getAdj (_, (x, y)) D = ('.', (x, y + 1))

getExitGrid :: (Int, Int) -> Dir -> [(Char, (Int, Int))]
getExitGrid (_, _) L = [('.', (-1, 0))]
getExitGrid (_, c) R = [('.', (c + 1, 0))]
getExitGrid (r, _) U = [('.', (0, r + 1))]
getExitGrid (_, _) D = [('.', (0, -1))]

isInline :: Dir -> (Char, (Int, Int)) -> (Char, (Int, Int)) -> Bool
isInline L (_, (x1, y1)) (_, (x2, y2)) = x2 < x1 && y1 == y2
isInline R (_, (x1, y1)) (_, (x2, y2)) = x2 > x1 && y1 == y2
isInline U (_, (x1, y1)) (_, (x2, y2)) = y2 > y1 && x1 == x2
isInline D (_, (x1, y1)) (_, (x2, y2)) = y1 > y2 && x1 == x2

dist :: (Char, (Int, Int)) -> Dir -> (Char, (Int, Int)) -> Int
dist (_, (x1, y1)) dir (_, (x2, y2))  =
    if dir == U || dir == D then abs (y2 - y1) else abs (x2 - x1)

rotate :: Dir -> Dir
rotate U = R
rotate R = D
rotate D = L
rotate L = U

findStart :: [[(Char, (Int, Int))]] -> (Char, (Int, Int))
findStart grid =
    case res of
        Just x -> x
        Nothing -> error "Borked"
    where res = find isStart (concat grid)

isStart :: (Char, (Int, Int)) -> Bool
isStart (c, _) = c == '^'

isInterestingChar :: (Char, (Int, Int)) -> Bool
isInterestingChar (c, _) = c `elem` ['#', '^']

mapWithIndices :: [[a]] -> [[(a, (Int, Int))]]
mapWithIndices matrix =
    [ [ (i, (c, r)) | (c, i) <- zip [0..] row ] | (r, row) <- zip [0..] (reverse matrix) ]