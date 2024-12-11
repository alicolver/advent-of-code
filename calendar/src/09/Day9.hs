module Day9 (
    day9
) where
import Data.Maybe (isNothing)
import Data.List (group, sortBy, scanl', find, findIndex)

digits :: String -> [Int]
digits = map (read . pure)

day9 :: IO ()
day9 = do
    input <- readFile "src/09/input.txt"
    let nums = digits input
    let expanded = solve nums 0 [] True
    let ords = solve' expanded
    let unwrapped = unwrap ords
    let zipped = zip unwrapped [0..]
    print $ calc zipped
    let withStartIndex = flattenedIndices (group expanded)
    let p2 = part2 (reverse (filter (notElem Nothing . snd) withStartIndex)) (group expanded)
    print p2
    let unwrapped2 = unwrap p2
    let zipped2 = zip unwrapped2 [0..]
    print $ calc zipped2

flattenedIndices :: [[Maybe Int]] -> [(Int, [Maybe Int])]
flattenedIndices lists =
  let headIndices = scanl' (+) 0 (map length lists)
  in zip (init headIndices) lists

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

part2 :: [(Int, [Maybe Int])] -> [[Maybe Int]] -> [Maybe Int]
part2 [] s = concat s
part2 (x:xs) state = part2 xs newState
    where
        findGap = filter (\a -> elem Nothing (snd a) && length (snd x) <= length (snd a) && fst a < fst x ) (flattenedIndices state)
        isGap = not (null findGap)
        newState = if isGap then swap x state else state

swap :: (Int, [Maybe Int]) -> [[Maybe Int]] -> [[Maybe Int]]
swap (index, justs) toInsertInto = group (concat (start ++ [justs] ++ [extras] ++ end))
    where
        start = takeWhile (\a -> notElem Nothing a || length a < length justs) toInsertInto
        nothings = head (filter (\a -> (Nothing `elem` a) && length a >= length justs) toInsertInto)
        extras = replicate (length nothings - length justs) Nothing
        unfinishedEnd = tail (dropWhile (\a -> notElem Nothing a || length a < length justs) toInsertInto)
        replaceNothing = replicate (length justs) Nothing
        startEnd = take (index - length (concat start) - length justs - length extras) (concat unfinishedEnd)
        endEnd = group (drop (index - length (concat start) - length justs - length extras) (concat unfinishedEnd))
        endEnd' = if not (null endEnd) then tail endEnd else []
        end = [startEnd] ++ [replaceNothing] ++ endEnd'


unwrap :: [Maybe Int] -> [Int]
unwrap [] = []
unwrap ((Just x):xs) = x : unwrap xs
unwrap (Nothing:xs) = 0 : unwrap xs

calc :: [(Int,Int)] -> Int
calc ps = sum (map (uncurry (*)) ps)
