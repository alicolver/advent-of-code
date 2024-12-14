module Day13(
    day13
) where
import Text.Parsec
import Text.Parsec.String
import Debug.Trace (trace)

data System = System {
    a :: (Integer,Integer),
    b :: (Integer,Integer),
    prize :: (Integer,Integer)
} deriving (Show,Eq)

int :: Parser Integer
int = read <$> many1 digit

system :: Parser System
system = do
    _ <- string "Button A: X+" >> spaces
    ax <- int
    _ <- string ", Y+" >> spaces
    ay <- int
    _ <- newline
    _ <- string "Button B: X+"
    bx <- int
    _ <- string ", Y+"
    by <- int
    _ <- newline
    _ <- string "Prize: X="
    prizex <- int
    _ <- string ", Y="
    prizey <- int
    return (System (ax,ay) (bx,by) (prizex, prizey))

systemsParser :: Parser [System]
systemsParser = system `sepBy` many1 newline

parseSystems :: String -> [System]
parseSystems input = case parse systemsParser "" input of
    Left err -> error (show err)
    Right result -> result

day13 :: IO()
day13 = do
    input <- readFile "src/13/input.txt"
    let systems = parseSystems input
    print systems
    print $ sum $ map solveSystem systems
    print $ sum $ map solveSystem (map p2multi systems)

p2multi :: System -> System
p2multi (System a b (px,py)) = System a b (px+10000000000000,py+10000000000000)

solveSystem :: System -> Integer
solveSystem (System (ax,ay) (bx,by) (px,py)) = calcSolution det (multiply inverse target)
    where
        matrix = trace (show [[ax,bx],[ay,by]]) (([[ax,bx],[ay,by]]))
        target = [px,py]
        det = determinant matrix
        inverse = [[by,-bx],[-ay,ax]]

calcSolution :: Integer -> [Integer] -> Integer
calcSolution det [a,b]
    | (a `mod` det) /= 0 = 0
    | (b `mod` det) /= 0 = 0
    | otherwise = (3* (a `div` det)) + (b `div` det)

multiply :: [[Integer]] -> [Integer] -> [Integer]
multiply [[ax,ay],[bx,by]] [px,py] = [((ax*px)+(ay*py)),(bx*px)+(by*py)]

determinant :: [[Integer]] -> Integer
determinant [[ax,ay],[bx,by]] = (ax * by) - (ay * bx)