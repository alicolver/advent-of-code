import Text.Regex.Posix
import Text.Read (readMaybe)

findMatches :: String -> String -> [String]
findMatches pattern input = getAllTextMatches (input =~ pattern) :: [String]

t :: String -> (Int,Int)
t s = listToTuple (map read groups)
    where (_, _, _, groups) = s =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: (String, String,String, [String])

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "List must have exactly two elements"

main :: IO ()
main = do
    input <- readFile "input.txt"
    let x =  (findMatches "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" input)
    print x
    let y = map t x
    print (day3 y)

day3 :: [(Int, Int)] -> Int
day3 x = sum (map (\(a, b) -> a * b) x)