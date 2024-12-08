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
    input <- readFile "src/07/input.txt"
    let psd = map parseEnt (lines input)
    print (sum (map target (filter part1 psd)))
    print (sum (map target (filter part2 psd)))

part1 :: Ent -> Bool
part1 (Ent t ns) = checkForward ns Nothing t False

part2 :: Ent -> Bool
part2 (Ent t ns) = checkForward ns Nothing t True

checkForward :: [Int] -> Maybe Int -> Int -> Bool -> Bool
checkForward [] acc t _ = acc == Just t
checkForward (a:as) Nothing t conOp = checkForward as (Just a) t conOp
checkForward (a:as) (Just acc) t conOp
    | a * acc > t = add || cons
    | otherwise = mul || add || cons
    where
        cons = conOp && checkForward as (Just (addDigits acc a)) t conOp
        add = checkForward as (Just (a+acc)) t conOp
        mul = checkForward as (Just (a*acc)) t conOp

addDigits :: Int -> Int -> Int
addDigits x y = read (show x ++ show y)