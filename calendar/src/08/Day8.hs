module Day8 (
    day8
) where
import Lib (mapWithIndices)
import Data.List (nub)

data Point = Point {
    val :: Char,
    coord :: (Int, Int)
} deriving (Show, Eq)

data Bounds = Bounds {
    rows :: Int,
    cols :: Int
}

day8 :: IO ()
day8 = do
    rawMap <- readFile "src/08/input.txt"
    let grid = lines rawMap
    let gridWithIndex = mapToPoint grid
    let antennas = concatMap (filter isAntenna) gridWithIndex
    let bounds = Bounds (length gridWithIndex) (length (head gridWithIndex))
    print (length (nub (concatMap (addPoints antennas [] bounds False) (concat gridWithIndex))))
    print (length (nub (concatMap (addPoints antennas [] bounds True) (concat gridWithIndex))))

isAntenna :: Point -> Bool
isAntenna (Point v _) = v /= '.'

addPoints :: [Point] -> [Point] -> Bounds -> Bool -> Point -> [Point]
addPoints [] x _ _ _ = x
addPoints (p2:p2s) x bs isP1 p1
    | inline p1 p2 = addPoints p2s (x ++ nps) bs isP1 p1
    | otherwise = addPoints p2s x bs isP1 p1
    where
        nps = if isP1 then addNewPoints p1 p2 else addAllPointsOnLine p1 (getGradient p1 p2) bs

inline :: Point -> Point -> Bool
inline (Point c1 (x1,y1)) (Point c2 (x2,y2)) = c1 == c2 && x1 /= x2 && y1 /= y2

getGradient :: Point -> Point -> (Int, Int)
getGradient (Point _ (x1,y1)) (Point _ (x2,y2)) = (x2 - x1, y2 - y1)

addAllPointsOnLine :: Point -> (Int, Int) -> Bounds -> [Point]
addAllPointsOnLine (Point _ (x, y)) (x1,y1) bs = addGrad (x,y) (x1,y1) [] bs ++ addGrad (x,y) (- x1,- y1) [] bs

addGrad :: (Int, Int) -> (Int, Int) -> [Point] -> Bounds -> [Point]
addGrad (x1, y1) (x2, y2) ps bs
    | inGrid bs np = addGrad (coord np) (x2, y2) (np : ps) bs
    | otherwise = ps
    where
        np = Point '+' (x1+x2, y1+y2)

addNewPoints :: Point -> Point -> [Point]
addNewPoints (Point _ (x1, y1)) (Point _ (x2, y2))
    | x1 == x2 = [Point '+' (x1, y1 - abs (y2 - y1)), Point '+' (x1, y2 + abs (y2 - y1))]
    | y1 == y2 = [Point '+' (x1 - abs (x1 - x2), y1), Point '+' (x2 + abs (x1 - x2), y2)]
    | y2 < y1 = []
    | otherwise =
        if x2 < x1
            then [Point '+' (x2 - abs (x1 - x2), y2 + abs (y1 - y2)), Point '+' (x1 + abs (x1 - x2), y1 - abs (y1 - y2))]
            else [Point '+' (x1 - abs (x1 - x2), y1 - abs (y1 - y2)), Point '+' (x2 + abs (x1 - x2), y2 + abs (y1 - y2))]

mapToPoint :: [[Char]] -> [[Point]]
mapToPoint matrix = map (map (uncurry Point)) (mapWithIndices matrix)

inGrid :: Bounds -> Point -> Bool
inGrid (Bounds rs cs) (Point _ (x, y)) = not (x < 0 || y < 0 || y >= rs || x >= cs)