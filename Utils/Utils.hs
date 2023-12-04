module Utils.Utils where

import Data.List

flatmap :: (t -> [a]) -> [t] -> [a]
flatmap _ [] = []
flatmap f (x:xs) = f x ++ flatmap f xs

sublists :: String -> [String]
sublists = concatMap (tail . inits) . tails