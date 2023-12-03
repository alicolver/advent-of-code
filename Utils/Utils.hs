module Utils.Utils where

flatmap :: (t -> [a]) -> [t] -> [a]
flatmap _ [] = []  
flatmap f (x:xs) = f x ++ flatmap f xs