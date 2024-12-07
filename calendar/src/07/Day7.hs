module Day7 (
    day7
) where

import Text.Parsec
import Text.Parsec.String

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
    input <- readFile "src/07/test.txt"
    let psd = map parseEnt (lines input)
    print (sum (map target (filter part1 psd)))

part1 :: Ent -> Bool
part1 (Ent t ns) = 0 == foldr apply t ns

apply :: Int -> Int -> Int
apply a b
    | a == b = 0
    | isFactor a b = b `div` a
    | otherwise = b - a

isFactor :: Int -> Int -> Bool
isFactor a b = b `mod` a == 0