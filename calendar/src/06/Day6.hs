module Day6 (
    day6
) where

import Data.List ( find, nub )
import Lib (mapWithIndices)

data Dir = U | L | D | R
    deriving (Eq, Show)

day6 :: IO ()
day6 = do
    rawMap <- readFile "src/06/input.txt"
    let grid = lines rawMap
    let gridWithIndex = mapWithIndices grid
    let (c, (x, y)) = findStart gridWithIndex
    let rcs = (length grid, length (head grid))
    let pointsOfInterest = map (filter isInterestingChar) gridWithIndex
    let visitedSpots = map fst (part1 pointsOfInterest (c, (x,y)) U rcs [((x,y),U)])
    print (length (nub visitedSpots))
    let potentialPois = map makePoi (nub visitedSpots)
    let totalNewPois = map (: concat pointsOfInterest) potentialPois
    print (length (filter (part2 (c, (x,y)) U rcs [((x,y),U)]) totalNewPois))

makePoi :: (Int,Int) -> (Char, (Int, Int))
makePoi (x,y) = ('#', (x, y))

part1 :: [[(Char, (Int, Int))]] -> (Char, (Int, Int)) -> Dir -> (Int, Int) -> [((Int, Int), Dir)]  -> [((Int,Int), Dir)]
part1 pois curPos dir rcs xs
    | isInGrid = part1 pois nextPos newDir rcs (((x1, y1), newDir) : xs)
    | otherwise = xs
    where
        (c, (x1, y1), newDir) = findNextPos curPos dir (concat pois)
        nextPos = (c, (x1, y1))
        isInGrid = inGrid rcs nextPos

part2 :: (Char, (Int, Int)) -> Dir -> (Int, Int) -> [((Int, Int), Dir)] -> [(Char, (Int, Int))] -> Bool
part2 curPos dir rcs xs pois = isInGrid && (alreadyTraversed || part2 nextPos newDir rcs (((x1, y1), newDir) : xs) pois)
    where
        (c, (x1, y1), newDir) = findNextPos curPos dir (pois)
        nextPos = (c, (x1, y1))
        isInGrid = inGrid rcs nextPos
        alreadyTraversed = ((x1, y1), newDir) `elem` xs

inGrid :: (Int, Int) -> (Char, (Int, Int)) -> Bool
inGrid (rs, cs) (_, (x, y)) = not (x < 0 || y < 0 || y >= rs || x >= cs)

findNextPos :: (Char, (Int, Int)) -> Dir -> [(Char, (Int, Int))] -> (Char, (Int, Int), Dir)
findNextPos (c, (x, y)) L potentials
    | ('#', (x-1, y)) `elem` potentials = findNextPos (c, (x, y)) U potentials
    | otherwise = ('.', (x-1, y), L)
findNextPos (c, (x, y)) R potentials
    | ('#', (x+1, y)) `elem` potentials = findNextPos (c, (x, y)) D potentials
    | otherwise = ('.', (x+1, y), R)
findNextPos (c, (x, y)) U potentials
    | ('#', (x, y+1)) `elem` potentials = findNextPos (c, (x, y)) R potentials
    | otherwise = ('.', (x, y+1), U)
findNextPos (c, (x, y)) D potentials
    | ('#', (x, y-1)) `elem` potentials = findNextPos (c, (x, y)) L potentials
    | otherwise = ('.', (x, y-1), D)

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