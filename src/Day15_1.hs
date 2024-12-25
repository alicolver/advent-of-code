module Day15_1(
    day15_1
) where
import Lib (mapWithIndices)
import Data.Maybe (isNothing)

data Command = U | R | D | L deriving (Show)
data Res = Res {
    ws :: [Point],
    bs :: [Point],
    g :: Point
}
type Point = (Int,Int)

day15_1 :: IO ()
day15_1 = do
    inputMap <- readFile "src/15/testMap.txt"
    coms <- readFile "src/15/testComs.txt"
    let grid = mapWithIndices (lines inputMap)
    let commands = map parseCommand (concat (lines coms))
    let walls = map snd (getAll '#' grid)
    let boxes = map snd (getAll 'O' grid)
    let weeGuy = snd $ head $ getAll '@' grid
    let points = p1' walls boxes weeGuy commands []
    -- mapM_ (printGrids (length (head grid), length grid)) points
    print $ p1 (length grid) (bs (last points))

printGrids :: (Int,Int) -> Res -> IO ()
printGrids (c,r) points  = do
    mapM_ putStrLn [[
        if (x, y) `elem` ws points
            then '#'
            else if (x,y) `elem` bs points
                then 'O'
                else if (x,y) == g points
                    then '@'
                    else '.' | x <- [0..c -1]] | y <- reverse [0..r -1]]
p1 :: Int -> [Point] -> Int
p1 ys boxes = sum $ map (\(x,y) -> ((ys-y-1)*100) + x) boxes
p1' :: [Point] -> [Point] -> Point -> [Command] -> [Res] -> [Res]
p1' walls boxes guy [] res = res ++ [Res walls boxes guy]
p1' walls boxes weeGuy (c:cs) res = p1' walls newBoxes newWeeGuy cs (res ++ [Res walls boxes weeGuy])
    where
        (newWeeGuy, newBoxes) = getNextPoint walls boxes weeGuy c
getNextPoint :: [Point] -> [Point] -> Point -> Command -> (Point, [Point])
getNextPoint walls boxes guy c
    | isFree = (newPoint, boxes)
    | isWall || cantShiftBoxes = (guy, boxes)
    | otherwise = (newPoint, newBoxes)
    where
        newPoint = applyToPoint guy c
        isWall = newPoint `elem` walls
        isFree = newPoint `notElem` boxes && newPoint `notElem` walls
        maybeNewElem = findGapOrNothing walls boxes newPoint c
        cantShiftBoxes = isNothing maybeNewElem
        newBoxes = if cantShiftBoxes then boxes else updateBoxes boxes newPoint maybeNewElem

updateBoxes :: [Point] -> Point -> Maybe Point -> [Point]
updateBoxes _ _ Nothing = error "Borked!"
updateBoxes boxes np (Just toSwap) = takeWhile (/= np) boxes ++ [toSwap] ++ end
    where
        dropped = dropWhile (/= np) boxes
        end = if null dropped then [] else tail dropped
findGapOrNothing :: [Point] -> [Point] -> Point -> Command -> Maybe Point
findGapOrNothing walls boxes boxPos c
    | isWall = Nothing
    | isBox = findGapOrNothing walls boxes np c
    | otherwise = Just np
    where
        np = applyToPoint boxPos c
        isWall = np `elem` walls
        isBox = np `elem` boxes

applyToPoint :: Point -> Command -> Point
applyToPoint (x,y) L = (x-1,y)
applyToPoint (x,y) R = (x+1,y)
applyToPoint (x,y) U = (x,y+1)
applyToPoint (x,y) D = (x,y-1)

getAll :: Char -> [[(Char, Point)]] -> [(Char, Point)]
getAll c = concatMap (filter (\a -> fst a == c))

parseCommand :: Char -> Command
parseCommand '^' = U
parseCommand '<' = L
parseCommand '>' = R
parseCommand 'v' = D
parseCommand _ = error "BORKED"
