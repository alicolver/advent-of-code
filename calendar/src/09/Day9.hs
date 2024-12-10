module Day9 (
    day9
) where
import Data.Maybe (isNothing, isJust)
import Data.List (group, sortBy)
import Debug.Trace (trace)
import Data.Map (Map)

digits :: String -> [Int]
digits = map (read . pure)

day9 :: IO ()
day9 = do
    input <- readFile "src/09/test.txt"
    let nums = digits input
    let expanded = solve nums 0 [] True
    let ords = solve' expanded
    let unwrapped = unwrap ords
    let zipped = zip unwrapped [0..]
    print $ calc zipped
    let p2 = part2 (group expanded)
    print p2
    let unwrapped2 = unwrap p2
    let zipped2 = zip unwrapped2 [0..]
    print $ calc zipped2

solve :: [Int] -> Int -> [Maybe Int] -> Bool -> [Maybe Int]
solve [] _ na _ = na
solve (x:xs) cur na True = solve xs (cur+1) (na ++ replicate x (Just cur)) False
solve (x:xs) cur na False = solve xs cur (na ++ replicate x Nothing) True

solve' :: [Maybe Int] -> [Maybe Int]
solve' [] = []
solve' (Nothing:xs) = newElem : solve' (reverse (tail filteredArray))
    where
        filteredArray = if isNothing (last xs) then dropWhile (==Nothing) (reverse xs) else reverse xs
        newElem = head filteredArray
solve' (x:xs) = x : solve' xs

part2 :: [[Maybe Int]] -> [Maybe Int]
part2 [] = []
part2 [x] = x
part2 (x:xs)
    | any isNothing x = part2 xs ++ x
    | any isNothing x && length x >= length (last xs) =
        part2 (group (fst swapped ++ nothings ++ concat (init xs))) ++ take (length (last xs)) x
    | otherwise = part2 (group (x ++ part2 (init xs))) ++ last xs
    where
        swapped = swapList x (last xs)
        nothings = replicate (snd swapped) Nothing

data Entry = Entry {
    size :: Int,
    val :: Maybe Int
} deriving(Eq, Show)

p2 :: [[Maybe Int]] -> [(Int, [Maybe Int])] -> [Maybe Int]
p2 (x:xs) [] = []
p2 (x:xs) (a:aggs) = []

createEntries :: [[Maybe Int]] -> [Entry]
createEntries = map (\a -> Entry (length a) (head a))

aggregateEntries :: [Entry] -> Int -> [(Int, [Maybe Int])]
aggregateEntries _ 10 = []
aggregateEntries x len = (len, map val (filter (\x -> size x == len) x)) : aggregateEntries x (len+1)



swapList :: [Maybe Int] -> [Maybe Int] -> ([Maybe Int], Int)
swapList x y = (y, length x- length y)

unwrap :: [Maybe Int] -> [Int]
unwrap [] = []
unwrap ((Just x):xs) = x : unwrap xs
unwrap (Nothing:xs) = 0 : unwrap xs

calc :: [(Int,Int)] -> Int
calc ps = sum (map (uncurry (*)) ps)
