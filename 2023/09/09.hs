import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = do
    input <- readFile "09/input.txt"
    let parsed = map parseInput (lines input)
    print (parsed)
    print (sum (map (extrapolate [] last False) parsed))
    print (sum (map (extrapolate [] head True) parsed))

extrapolate :: [[Int]] -> ([Int] -> Int) -> Bool -> [Int] -> Int
extrapolate ss f isNeg current = 
    if (all (==0) current) 
    then extrapolate' ss f isNeg 0
    else extrapolate (current : ss) f isNeg (differences current) 

extrapolate' :: [[Int]] -> ([Int] -> Int) -> Bool -> Int -> Int
extrapolate' (x:[]) f isNeg prevDiff = f x + (if isNeg then -1 * prevDiff else prevDiff)
extrapolate' (x:xs) f isNeg prevDiff = extrapolate' xs f isNeg (f x + (if isNeg then -1 * prevDiff else prevDiff))

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (x:y:xs) = (y - x) : differences (y : xs) 

parseInput :: String -> [Int]
parseInput input = case parse parseSeq "" input of
    Left err -> error (show err)
    Right result -> result

parseSeq :: Parser [Int]
parseSeq = sepBy integer space

integer :: Parser Int
integer = do
    sign <- option 1 (char '-' >> return (-1))
    value <- read <$> many1 digit
    return (sign * value)