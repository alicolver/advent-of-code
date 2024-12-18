module Day15(
    day15
) where
import Lib (mapWithIndices)
import Data.Maybe (isNothing)

data Command = U | R | D | L deriving (Show, Eq)
data Res = Res {
    ws :: [Point],
    bs :: [Point],
    g :: Point
}
type Point = (Char, (Int,Int))

day15 :: IO ()
day15 = do
    inputMap <- readFile "src/input/test_map.txt"
    coms <- readFile "src/input/test_coms.txt"
    let grid = mapWithIndices (lines inputMap)
    let commands = map parseCommand (concat (lines coms))
    let walls = getAll '#' grid
    let boxes = getAll 'O' grid
    let weeGuy = head $ getAll '@' grid
    let points = p1' walls boxes weeGuy commands []
    mapM_ (printGrids (length (head grid), length grid)) points
    print $ p1 (length grid) (bs (last points))
    let expanded = map expandRow (lines inputMap)
    let p2Grid = mapWithIndices expanded
    let p2Boxes = getAll '[' p2Grid ++ getAll ']' p2Grid
    let p2walls = getAll '#' p2Grid
    let p2weeGuy = head $ getAll '@' p2Grid
    let p2points = p2' p2walls p2Boxes p2weeGuy commands []
    print $ p1 (length p2Grid) (bs (last p2points))

printGrids :: (Int,Int) -> Res -> IO ()
printGrids (c,r) points  = do
    mapM_ putStrLn [[
        if (x, y) `elem` map snd (ws points)
            then '#'
            else if (x,y) `elem` map snd (bs points)
                then 'O'
                else if (x,y) == snd (g points)
                    then '@'
                    else '.' | x <- [0..c -1]] | y <- reverse [0..r -1]]

p1 :: Int -> [Point] -> Int
p1 ys boxes = sum $ map (\(_, (x,y)) -> ((ys-y-1)*100) + x) boxes

expandRow :: [Char] -> [Char]
expandRow [] = []
expandRow (c:cs)
    | c == '#' = "##" ++ expandRow cs
    | c == '.' = ".." ++ expandRow cs
    | c == 'O' = "[]" ++ expandRow cs
    | c == '@' = "@." ++ expandRow cs
    | otherwise = error "Borked!"

p1' :: [Point] -> [Point] -> Point -> [Command] -> [Res] -> [Res]
p1' walls boxes guy [] res = res ++ [Res walls boxes guy]
p1' walls boxes weeGuy (c:cs) res = p1' walls newBoxes newWeeGuy cs (res ++ [Res walls boxes weeGuy])
    where
        (newWeeGuy, newBoxes) = getNextPoint walls boxes weeGuy c

p2' :: [Point] -> [Point] -> Point -> [Command] -> [Res] -> [Res]
p2' walls boxes guy [] res = res ++ [Res walls boxes guy]
p2' walls boxes weeGuy (c:cs) res = p2' walls newBoxes newWeeGuy cs (res ++ [Res walls boxes weeGuy])
    where
        (newWeeGuy, newBoxes) = p2'' walls boxes weeGuy c

p2'' :: [Point] -> [Point] -> Point -> Command -> (Point, [Point])
p2'' walls boxes guy c
    | isFree = (newPoint, boxes)
    | isWall = (guy, boxes)
    | not canPush = (guy, boxes)
    | otherwise = (newPoint, newBoxes)
    where
        isFree = snd newPoint `notElem` map snd boxes && snd newPoint `notElem` map snd walls
        newPoint = applyToPoint guy c
        isWall = snd newPoint `elem` map snd walls
        canPush = canPushBox walls boxes newPoint c
        newBoxes = if canPush then updateBoxes' walls boxes newPoint c else boxes

updateBoxes' :: [Point] -> [Point] -> Point -> Command -> [Point]
updateBoxes' walls boxes boxPos@(ch, (x,y)) c
    | not isBox = []
    | c == L = (ch, (x-1,y)) : updateBoxes' walls boxes (nlChar, (x-1, y)) c
    | c == R = (ch, (x+1,y)) : updateBoxes' walls boxes (nlChar, (x+1, y)) c
    | otherwise = []
    where
        nlChar = if ch == '[' then ']' else '['
        isBox = snd boxPos `elem` map snd boxes

getNextPoint :: [Point] -> [Point] -> Point -> Command -> (Point, [Point])
getNextPoint walls boxes guy c
    | isFree = (newPoint, boxes)
    | isWall || cantShiftBoxes = (guy, boxes)
    | otherwise = (newPoint, newBoxes)
    where
        newPoint = applyToPoint guy c
        isWall = snd newPoint `elem` map snd walls
        isFree = snd newPoint `notElem` map snd boxes && snd newPoint `notElem` map snd walls
        maybeNewElem = findGapOrNothing walls boxes newPoint c
        cantShiftBoxes = isNothing maybeNewElem
        newBoxes = if cantShiftBoxes then boxes else updateBoxes boxes newPoint maybeNewElem

updateBoxes :: [Point] -> Point -> Maybe Point -> [Point]
updateBoxes _ _ Nothing = error "Borked!"
updateBoxes boxes np (Just toSwap) = takeWhile (\a -> snd a /= snd np) boxes ++ [toSwap] ++ end
    where
        dropped = dropWhile (\a -> snd a /= snd np) boxes
        end = if null dropped then [] else tail dropped

canPushBox :: [Point] -> [Point] -> Point -> Command -> Bool
canPushBox walls boxes boxPos@(ch, (x,y)) c
    | isWall = False
    | isBox && (c == L || c == R) = canPushBox walls boxes np c
    | isBox = canPushBox walls boxes np c && canPushBox walls boxes nnp c
    | otherwise = True
    where
        np = applyToPoint boxPos c
        nnp = if fst np == ']' then applyToPoint (ch, (x-1,y)) c else applyToPoint (ch, (x+1,y)) c
        isWall = snd np `elem` map snd walls
        isBox = snd np `elem` map snd boxes

findGapOrNothing :: [Point] -> [Point] -> Point -> Command -> Maybe Point
findGapOrNothing walls boxes boxPos c
    | isWall = Nothing
    | isBox = findGapOrNothing walls boxes np c
    | otherwise = Just np
    where
        np = applyToPoint boxPos c
        isWall = snd np `elem` map snd walls
        isBox = snd np `elem` map snd boxes

applyToPoint :: Point -> Command -> Point
applyToPoint (c, (x,y)) L = (c, (x-1,y))
applyToPoint (c, (x,y)) R = (c, (x+1,y))
applyToPoint (c, (x,y)) U = (c, (x,y+1))
applyToPoint (c, (x,y)) D = (c, (x,y-1))

getAll :: Char -> [[Point]] -> [Point]
getAll c = concatMap (filter (\a -> fst a == c))

parseCommand :: Char -> Command
parseCommand '^' = U
parseCommand '<' = L
parseCommand '>' = R
parseCommand 'v' = D
parseCommand _ = error "BORKED"