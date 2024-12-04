import Text.Parsec
import Text.Parsec.String
import Data.Char

data TimeDistance = TimeDistance {
    times :: [Int],
    distances :: [Int]
} deriving Show

main :: IO ()
main = do
    input <- readFile "06/input.txt"
    let x = parseInput input
    let zipped = zip (times x) (distances x)
    print (product (map calculate zipped))
    let totalTime = joiner (map fst zipped)
    let totalDistance = joiner (map snd zipped)
    print (calculate (totalTime, totalDistance))

calculate :: (Int, Int) -> Int
calculate (time, distance) = length (filter (> distance) [timeLeft * (time - timeLeft) | timeLeft <- [0..time]])

parseInput :: String -> TimeDistance
parseInput input = case parse parseTimeDistance "" input of
    Left err -> error (show err)
    Right result -> result

parseTimeDistance :: Parser TimeDistance
parseTimeDistance = TimeDistance
    <$> (string "Time:" *> spaces *> sepEndBy1 integer (many1 (char ' ')) <* newline)
    <*> (string "Distance:" *> spaces *> sepEndBy1 integer (many1 (char ' ')))

integer :: Parser Int
integer = read <$> many1 digit

joiner :: [Int] -> Int
joiner = read . concatMap show