import Text.Parsec.String
import Text.Parsec
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "2024/input/01.txt"
    let rows = map parser ( lines input )
    print  ( day1  rows )
    print (day1' (unzip rows))

day1 :: [(Int, Int)] -> Int
day1 x = sum (map diff ( sortTuple ( unzip x ) ) )

day1' :: ([Int], [Int]) -> Int 
day1' (l,r) = sum (map (multi 0 r) l)

sortTuple :: ([Int], [Int]) -> [(Int, Int)]
sortTuple (l, r) = zip (sort l) (sort r)

diff :: (Int, Int) -> Int
diff (a, b) =  abs (a - b)

multi :: Int -> [Int] -> Int -> Int
multi  x [] t = x 
multi  x (h:hs) t = if t == h then (multi (x+h) hs t) else (multi x hs t)

parser :: String -> (Int, Int)
parser input = case parse intPairParser "" input of
    Left err -> error (show err)
    Right result -> result

intPairParser :: Parser (Int, Int)
intPairParser = (,) <$> (read <$> many1 digit) <* many space <*> (read <$> many1 digit)
