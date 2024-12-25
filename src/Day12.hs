module Day12(
    day12
) where
import Lib (mapWithIndices)
import Data.Maybe (catMaybes)

type Coord = (Int,Int)
type PlantLoc = (Char, Coord)
data Dir = L | U | R | D
    deriving (Eq,Show)

day12 :: IO()
day12 = do
    input <- readFile "src/input/12.txt"
    let farm = mapWithIndices $ lines input
    let groups = getSections farm
    let coords = map (map toCoord) groups
    print $ p1 coords
    print $ p2 coords

p1 :: [[Coord]] -> Int
p1 coords = sum (map areaByPerimeter coords)

p2 :: [[Coord]] -> Int
p2 coords = sum (map areaBySides coords)

areaByPerimeter :: [Coord] -> Int
areaByPerimeter cs = area cs * sum (map (perimeter cs) cs)

areaBySides :: [Coord] -> Int
areaBySides cs = area cs * sides cs cs

sides :: [Coord] -> [Coord] -> Int
sides _ [] = 0
sides shape ((x,y):cs) = total + sides shape cs
    where
        tl = if (x-1,y) `notElem` shape && (x,y+1) `notElem` shape then 1 else 0
        tr = if (x+1,y) `notElem` shape && (x,y+1) `notElem` shape then 1 else 0
        br = if (x+1,y) `notElem` shape && (x,y-1) `notElem` shape then 1 else 0
        bl = if (x-1,y) `notElem` shape && (x,y-1) `notElem` shape then 1 else 0
        itl = if (x-1,y) `elem` shape && (x,y+1) `elem` shape && (x-1,y+1) `notElem` shape then 1 else 0
        itr = if (x+1,y) `elem` shape && (x,y+1) `elem` shape && (x+1,y+1) `notElem` shape then 1 else 0
        ibr = if (x+1,y) `elem` shape && (x,y-1) `elem` shape && (x+1,y-1) `notElem` shape then 1 else 0
        ibl = if (x-1,y) `elem` shape && (x,y-1) `elem` shape && (x-1,y-1) `notElem` shape then 1 else 0
        total = tl + tr + br + bl + itl + itr + ibr + ibl

area :: [Coord] -> Int
area = length

perimeter :: [Coord] -> Coord -> Int
perimeter fullShape (x,y) = length $ filter (`notElem` fullShape) (getAdj (x,y))

toCoord :: PlantLoc -> Coord
toCoord (_,(x,y)) = (x,y)

getSections :: [[PlantLoc]] -> [[PlantLoc]]
getSections farm = getAllSections farm [] (concat farm) []

getAllSections :: [[PlantLoc]] -> [PlantLoc] -> [PlantLoc] -> [[PlantLoc]] -> [[PlantLoc]]
getAllSections _ _ [] groups = groups
getAllSections farm visited (h:hs) groups
    | h `elem` visited = getAllSections farm visited hs groups
    | otherwise = getAllSections farm newVisited hs (res : groups)
    where
        res = getNeighbours farm [] h
        newVisited = visited ++ res

getNeighbours :: [[PlantLoc]] -> [PlantLoc] -> PlantLoc -> [PlantLoc]
getNeighbours farm visited cur@(c, (x,y))
    | cur `elem` visited = visited
    | null notVisited = visWithCur
    | otherwise = foldl processPoint visWithCur notVisited
    where
        visWithCur = cur : visited
        udlr = map (getLoc farm) (getAdj (x,y))
        pointsInGrid = catMaybes udlr
        filterMatch = filter (isMatching cur) pointsInGrid
        notVisited = filter (\a -> notElem a visited) filterMatch
        processPoint = getNeighbours farm

getAdj :: Coord -> [Coord]
getAdj (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

isMatching :: PlantLoc -> PlantLoc -> Bool
isMatching (c1, _) (c2, _) = c1 == c2

getLoc :: [[PlantLoc]] -> Coord -> Maybe PlantLoc
getLoc farm (x,y)
    | length farm <= y || y < 0 = Nothing
    | length (head farm) <= x || x < 0 = Nothing
    | otherwise = Just (farm !! y !! x)