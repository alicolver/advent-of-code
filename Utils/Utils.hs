module Utils.Utils where
import qualified Data.Map as Map
import Data.List

flatmap :: (t -> [a]) -> [t] -> [a]
flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs

sublists :: String -> [String]
sublists = concatMap (tail . inits) . tails

frequencyMap :: (Ord a) => [a] -> Map.Map a Int
frequencyMap = foldr (\x -> Map.insertWith (+) x 1) Map.empty

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

adjacentPairs :: [Int] -> [(Int, Int)]
adjacentPairs [x] = []
adjacentPairs (x:xs) = zip (x:xs) (xs)