module Day5 (
    day5
) where

import Data.Graph
import Data.List
import Text.Parsec
import Text.Parsec.String

day5 :: IO ()
day5 = do
    rawRules <- readFile "src/05/rules.txt"
    rawRoutes <- readFile "src/05/routes.txt"
    let rules = map parseRules (lines rawRules)
    let routes = map parseRoute (lines rawRoutes)
    print (part1 rules routes)
    print (part2 rules routes)

rule :: Parser (Int, Int)
rule = do
    prev <- read <$> many1 digit
    _ <- char '|'
    next <- read <$> many1 digit
    return (prev, next)

parseRules :: String -> (Int, Int)
parseRules input = case parse rule "" input of
    Left err -> error (show err)
    Right result -> result

route :: Parser [Int]
route = (read <$> many1 digit) `sepBy` char ','

parseRoute :: String -> [Int]
parseRoute input = case parse route "" input of
    Left err -> error (show err)
    Right result -> result

part1 :: [(Int,Int)] -> [[Int]] -> Int
part1 rules routes = sum (map getMid (filter (isValid rules) routes))

isValid :: [(Int, Int)] -> [Int] -> Bool
isValid rules xs = all (\(r1, r2) -> isValidPair rules r1 r2) (zip xs (tail xs))

isValidPair :: [(Int, Int)] -> Int -> Int -> Bool
isValidPair rules r1 r2 = all (\(a, b) -> not (a == r2 && b == r1)) rules

part2 :: [(Int, Int)] -> [[Int]] -> Int
part2 rules routes = sum (map getMid sortedRoutes)
    where
        invalidRoutes = filter (not . isValid rules) routes
        sortedRoutes = map (sortBy (sortByRule rules)) invalidRoutes

sortByRule :: [(Int,Int)] -> Int -> Int -> Ordering
sortByRule rules a b
    | (b, a) `elem` rules = GT
    | otherwise = EQ

getMid :: [a] -> a
getMid xs = xs !! ((length xs) `div` 2)