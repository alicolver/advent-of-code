module Day15(
    day15
) where
import Lib (mapWithIndices)
import qualified Data.Map as Map
import Data.Tuple (swap)

data Command = U | R | D | L deriving (Show, Eq)
type Point = (Int, Int)

simulate :: Map.Map (Int,Int) Char -> Point -> [Command] -> [Map.Map (Int,Int) Char]
simulate _ _ [] = []
simulate m p (c:cs) = nm : simulate nm guy cs
    where
        (nm, guy) = simulate' m p c

simulate' :: Map.Map (Int,Int) Char -> Point -> Command -> (Map.Map (Int,Int) Char, Point)
simulate' m g@(x,y) c
    | nextIsFree = (Map.insert nextMovePoint '@' mapWithoutGuy, nextMovePoint)
    | nextIsWall = (m, g)
    | not canPushBox = (m, g)
    | otherwise = (Map.insert nextMovePoint '@' updateMapBoxes, nextMovePoint)
    where
        nextMovePoint@(x1,y1) = applyToPoint c g
        nextIsFree = not $ Map.member nextMovePoint m
        nextMoveChar = m Map.! nextMovePoint
        mapWithoutGuy = Map.delete (x,y) m
        nextIsWall = nextMoveChar == '#'
        otherHalfOfBox = if nextMoveChar == ']' then (x1-1,y1) else (x1+1,y1)
        canPushBox = if c == L || c == R
            then canPushBoxLR m nextMovePoint c
            else canPushBoxUD m (nextMovePoint, otherHalfOfBox) c
        updateMapBoxes = if c == L || c == R
            then updateBoxesLR mapWithoutGuy nextMovePoint c
            else updateBoxesUD mapWithoutGuy (nextMovePoint, otherHalfOfBox) c

canPushBoxLR :: Map.Map (Int, Int) Char -> Point -> Command -> Bool
canPushBoxLR m p@(x,y) c
    | isEmptySpace = True
    | isWall = False
    | otherwise = canPushBoxLR m (applyToPoint c p) c
    where
        isEmptySpace = not $ Map.member (x,y) m
        isWall = m Map.! (x,y) == '#'

canPushBoxUD :: Map.Map (Int, Int) Char -> (Point,Point) -> Command -> Bool
canPushBoxUD m (p1,p2) c
    | isEmptySpace = True
    | isWall = False
    | otherwise = box1Pushable && box2Pushable
    where
        p1'@(x1,y1) = applyToPoint c p1
        p2'@(x2,y2) = applyToPoint c p2
        isEmptySpace = not (Map.member p1' m) && not (Map.member p2' m)
        c1 = if Map.member p1' m then Just (m Map.! p1') else Nothing
        c2 = if Map.member p2' m then Just (m Map.! p2') else Nothing
        isWall = c1 == Just '#' || c2 == Just '#'
        isBoxAbove1 = c1 == Just '[' || c1 == Just ']'
        isBoxAbove2 = c2 == Just '[' || c2 == Just ']'
        p1'' = if c1 == Just '[' then (x1+1,y1) else (x1-1,y1)
        p2'' = if c2 == Just '[' then (x2+1,y2) else (x2-1,y2)
        box1Pushable = not isBoxAbove1 || canPushBoxUD m (p1',p1'') c
        box2Pushable = not isBoxAbove2 || canPushBoxUD m (p2',p2'') c

updateBoxesUD :: Map.Map Point Char -> (Point, Point) -> Command -> Map.Map Point Char
updateBoxesUD m (p1, p2) c
    | isEmptySpace = m
    | otherwise =
        Map.insert p1' (m Map.! p1) $
        Map.insert p2' (m Map.! p2) updatedMapAfterP2
    where
        p1'@(x1, y1) = applyToPoint c p1
        p2'@(x2, y2) = applyToPoint c p2
        isEmptySpace = not (Map.member p1 m) && not (Map.member p2 m)
        c1 = if Map.member p1' m then Just (m Map.! p1') else Nothing
        c2 = if Map.member p2' m then Just (m Map.! p2') else Nothing
        isBoxAbove1 = c1 == Just '[' || c1 == Just ']'
        isBoxAbove2 = c2 == Just '[' || c2 == Just ']'
        p1'' = if c1 == Just '[' then (x1+1,y1) else (x1-1,y1)
        p2'' = if c2 == Just '[' then (x2+1,y2) else (x2-1,y2)
        updatedMapAfterP1 =
            if isBoxAbove1
                then updateBoxesUD (Map.delete p1 m) (p1', p1'') c
                else Map.delete p1 m
        updatedMapAfterP2 =
            if isBoxAbove2
                then updateBoxesUD (Map.delete p2 updatedMapAfterP1) (p2', p2'') c
                else Map.delete p2 updatedMapAfterP1

updateBoxesLR :: Map.Map Point Char -> Point -> Command -> Map.Map Point Char
updateBoxesLR m p c
    | not (Map.member p m) = m
    | otherwise = Map.insert newP currentChar updatedMap
    where
        currentChar = m Map.! p
        newP = applyToPoint c p
        mapWithoutCurrentPiece = Map.delete p m
        updatedMap = updateBoxesLR mapWithoutCurrentPiece newP c

printGrids :: Point -> Map.Map Point Char -> IO ()
printGrids (c,r) points  = mapM_ putStrLn [[
                            if Map.member (x,y) points
                                then points Map.! (x,y)
                                else '.'
                            | x <- [0..c -1]] | y <- reverse [0..r -1]
    ]

solve :: Int -> [Point] -> Int
solve ys boxes = sum $ map (\(x,y) -> (ys-y-1)*100 + x) boxes

expandRow :: String -> String
expandRow [] = []
expandRow (c:cs)
    | c == '#' = "##" ++ expandRow cs
    | c == '.' = ".." ++ expandRow cs
    | c == 'O' = "[]" ++ expandRow cs
    | c == '@' = "@." ++ expandRow cs
    | otherwise = error "Borked!"

applyToPoint :: Command -> Point -> Point
applyToPoint L (x,y) = (x-1,y)
applyToPoint R (x,y) = (x+1,y)
applyToPoint U (x,y) = (x,y+1)
applyToPoint D (x,y) = (x,y-1)

-- parsing etc

day15 :: IO ()
day15 = do
    inputMap <- readFile "src/input/15_map.txt"
    coms <- readFile "src/input/15_coms.txt"
    let commands = map parseCommand (concat (lines coms))
    let (grid, startGuy, rows) = setUpGrid inputMap
    let grids = simulate grid startGuy commands
    -- mapM_ (printGrids (length (head p2Grid), length p2Grid)) grids
    print $ solve rows (getLeftSideOfBoxes grids)

setUpGrid :: String -> (Map.Map Point Char, Point, Int)
setUpGrid inputMap = (grid, startGuy, length p2Grid)
    where
        p2Grid = mapWithIndices $ map expandRow (lines inputMap)
        filteredP2Grid = concatMap (filter (\a -> fst a /= '.')) p2Grid
        grid = Map.fromList $ map swap filteredP2Grid
        startGuy = snd $ head $ concatMap (filter (\a -> fst a == '@')) p2Grid

getLeftSideOfBoxes :: [Map.Map Point Char] -> [Point]
getLeftSideOfBoxes grids = map fst leftBoxOnly
    where
        mapAsList = Map.toList $ last grids
        leftBoxOnly = filter (\a -> snd a == '[') mapAsList

parseCommand :: Char -> Command
parseCommand '^' = U
parseCommand '<' = L
parseCommand '>' = R
parseCommand 'v' = D
parseCommand _ = error "BORKED"