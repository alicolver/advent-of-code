module Day7 (
    day7
) where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace

data Ent = Ent {
    target :: Int,
    nums :: [Int]
} deriving (Show)

ent :: Parser Ent
ent = do
    t <- read <$> many1 digit
    _ <- char ':'
    _ <- char ' '
    ns <- (read <$> many1 digit) `sepBy` space
    return (Ent t ns)

parseEnt :: String -> Ent
parseEnt input = case parse ent "" input of
    Left err -> error (show err)
    Right result -> result

day7 :: IO ()
day7 = do
    input <- readFile "src/07/input.txt"
    let psd = map parseEnt (lines input)
    print (sum (map target (filter part1 psd)))
    print (sum (map target (filter part2 psd)))

part1 :: Ent -> Bool
part1 (Ent t ns) = recursiveCheck (reverse ns) t

recursiveCheck :: [Int] -> Int -> Bool
recursiveCheck [] _ = False
recursiveCheck [a] t = a == t
recursiveCheck (a:as) t
    | isFactor a t = recursiveCheck (as) (t `div` a) || recursiveCheck (as) (t - a)
    | otherwise = recursiveCheck (as) (t - a)

addDigit :: Int -> Int -> Int
addDigit x y = (read ((show x) ++ (show y)))

isFactor :: Int -> Int -> Bool
isFactor a b = b `mod` a == 0