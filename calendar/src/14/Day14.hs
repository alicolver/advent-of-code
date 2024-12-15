module Day14(
    day14
) where
import Text.Parsec
import Text.Parsec.String
import Data.Maybe (isNothing)
import Debug.Trace
import Data.List (sort, group, minimumBy)
import Data.Ord (comparing)

data Quad = TL | TR | BR | BL | M
    deriving (Eq, Show, Ord)
data Bot = Bot {
    p :: (Int,Int),
    v :: (Int,Int)
} deriving (Show, Eq, Ord)
type Bounds = (Int,Int)

day14 :: IO ()
day14 = do
    input <- readFile "src/14/input.txt"
    let bots = parseBots input
    let bounds = (101,103)
    let newBots = p1Move bots bounds 100
    print (getQuads newBots bounds)
    p2 bots bounds

p1Move :: [Bot] -> (Int,Int) -> Int -> [Bot]
p1Move bs bounds n = map (moveBot bounds n) bs

p2 :: [Bot] -> (Int,Int) -> IO ()
p2 bots (c,r) = do
    print $ show $ snd minScore
    mapM_ print [[if (x, y) `elem` map p (p1Move bots (c,r) (snd minScore)) then '#' else '.' | x <- [0..c - 1]] | y <- [0..(r - 1)]]
    where
        allScores = [(getQuads (p1Move bots (c,r) i) (c,r), i) | i <- [0..(c*r)]]
        minScore = minimumBy (comparing fst) allScores

getQuads :: [Bot] -> Bounds -> Int
getQuads bots bounds = product (map length botsByQuads)
    where
        botsInQuads = map (quadForBot bounds) bots
        testThing = zip bots botsInQuads
        botsByQuads = group (sort (filter (/= M) (map snd testThing)))

quadForBot :: Bounds -> Bot -> Quad
quadForBot  (bx,by) (Bot (x,y) _)
    | x < halfX && y < halfY = TL
    | x > halfX && y < halfY = TR
    | x < halfX && y > halfY = BL
    | x > halfX && y > halfY = BR
    | otherwise = M
    where
        halfX = bx `div` 2
        halfY = by `div` 2

moveBot :: Bounds -> Int -> Bot -> Bot
moveBot (c,r) n (Bot (px,py) (vx,vy)) = Bot ((px + (vx * n)) `mod` c, (py + vy * n) `mod` r) (vx,vy)

int :: Parser Int
int = do
    sign <- optionMaybe (char '-')
    val <- many1 digit
    return (if isNothing sign then read val else - (read val))

botParser :: Parser Bot
botParser = do
    _ <- string "p="
    px <- int
    _ <- char ','
    py <- int
    _ <- string " v="
    vx <- int
    _ <- char ','
    vy <- int
    return (Bot (px,py) (vx,vy))

botsParser :: Parser [Bot]
botsParser = botParser `sepBy` newline

parseBots :: String -> [Bot]
parseBots input = case parse botsParser "" input of
    Left err -> error (show err)
    Right result -> result