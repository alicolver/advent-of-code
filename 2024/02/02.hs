import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "2024/02/input.txt"
    let rows = map (words) ( lines input )
    let ints = map (map read) rows
    print (day2 ints)
    print (day2' ints)

day2 :: [[Int]] -> Int
day2 x = length (filter isValid x)

day2' :: [[Int]] -> Int
day2' x = length (filter id (map (any isValid . removeOne) x))

isValid :: [Int] -> Bool
isValid x =  all isValidPair (adjacentPairs ( x )) || all isValidPair (adjacentPairs (reverse x))

isValidPair :: (Int, Int) -> Bool
isValidPair (a, b) = b > a && b - a >= 1 && b - a <= 3

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

adjacentPairs :: [Int] -> [(Int, Int)]
adjacentPairs [x] = []
adjacentPairs (x:xs) = zip (x:xs) (xs)