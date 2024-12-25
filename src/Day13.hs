module Day13(
    day13
) where
import Text.Parsec
import Text.Parsec.String
import Lib (intParser)

data System = System {
    a :: (Int,Int),
    b :: (Int,Int),
    prize :: (Int,Int)
} deriving (Show,Eq)

p2multi :: System -> System
p2multi (System a b (px,py)) = System a b (px+10000000000000,py+10000000000000)

solveSystem :: System -> Int
solveSystem (System (ax,ay) (bx,by) (px,py)) = calcSolution det $ multiply inverse target
    where
        matrix = [[ax,bx],[ay,by]]
        target = [px,py]
        det = determinant matrix
        inverse = [[by,-bx],[-ay,ax]]

calcSolution :: Int -> [Int] -> Int
calcSolution det [a,b]
    | (a `mod` det) /= 0 = 0
    | (b `mod` det) /= 0 = 0
    | otherwise = (3 * (a `div` det)) + (b `div` det)

multiply :: [[Int]] -> [Int] -> [Int]
multiply [[ax,ay],[bx,by]] [px,py] = [(ax*px)+(ay*py),(bx*px)+(by*py)]

determinant :: [[Int]] -> Int
determinant [[ax,ay],[bx,by]] = (ax * by) - (ay * bx)


-- parsing and orchestration

system :: Parser System
system = do
    let parseCoordinates prefix xLabel yLabel = do
            _ <- string prefix >> string xLabel
            x <- intParser
            _ <- string ", " >> string yLabel
            y <- intParser
            return (x, y)
    (ax, ay) <- parseCoordinates "Button A: " "X+" "Y+"
    _ <- newline
    (bx, by) <- parseCoordinates "Button B: " "X+" "Y+"
    _ <- newline
    (prizex, prizey) <- parseCoordinates "Prize: " "X=" "Y="
    return (System (ax, ay) (bx, by) (prizex, prizey))

systemsParser :: Parser [System]
systemsParser = system `sepBy` many1 newline

parseSystems :: String -> [System]
parseSystems input = case parse systemsParser "" input of
    Left err -> error (show err)
    Right result -> result

day13 :: IO()
day13 = do
    input <- readFile "src/input/13.txt"
    let systems = parseSystems input
    print systems
    print $ sum $ map solveSystem systems
    print $ sum $ map (solveSystem . p2multi) systems