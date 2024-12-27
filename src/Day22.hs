module Day22(
    day22
) where
import Text.Parsec ( parse )
import Lib (intParser, slidingWindow)
import Data.Bits ( Bits(xor) )
import qualified Data.Map as Map
import qualified Data.Set as Set

simulateN :: Int -> Int -> [Int]
simulateN n secret = take (n+1) $ iterate simulate secret

simulate :: Int -> Int
simulate secret = step3res
    where
        step1res = step1 secret
        step2res = step2 step1res
        step3res = step3 step2res

step1, step2, step3 :: Int -> Int
step1 secret = prune $ mix (secret * 64) secret
step2 secret = prune $ mix (secret `div` 32) secret
step3 secret = prune $ mix (secret * 2048) secret

mix :: Int -> Int -> Int
mix val secret = val `xor` secret

prune :: Int -> Int
prune secret = secret `mod` 16777216

getDiffs :: [Int] -> [Int]
getDiffs xs = zipWith (-) (tail xs) xs

p2 :: [[Int]] -> Int
p2 results = p2' combinedMap allWindows 0
    where
        windows = map (slidingWindow 4 . getDiffs) results
        resultsWithoutStarts = map (drop 4) results
        mappedResultWindows = zipWith (curry zipWindowResult) windows resultsWithoutStarts
        maps = map Map.fromList mappedResultWindows
        combinedMap = Map.unionsWith (+) maps
        allWindows = Set.toList $ Set.fromList (concat windows)

zipWindowResult :: ([[Int]], [Int]) -> [([Int], Int)]
zipWindowResult (ws, rs) = zip ws rs

p2' :: Map.Map [Int] Int -> [[Int]] -> Int -> Int
p2' _ [] m = m
p2' x (w:ws) m = p2' x ws $ max windowsMax m
    where
        windowsMax = Map.findWithDefault 0 w x

day22 :: IO ()
day22 = do
    input <- readFile "src/input/22.txt"
    let intArray = map parseInts $ lines input
    let results = map (simulateN 2000) intArray
    print $ sum $ map last results
    print $ p2 $ map (map (`mod` 10)) results

parseInts :: String -> Int
parseInts input = case parse intParser "" input of
    Left err -> error (show err)
    Right result -> result