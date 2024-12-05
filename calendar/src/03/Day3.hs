module Day3 (
    day3
) where

import Text.Regex.Posix

data Instr = Mul Int Int | Do | Dont
    deriving (Eq, Show)

findMatches :: String -> String -> [String]
findMatches pattern input = getAllTextMatches (input =~ pattern) :: [String]

t :: String -> Maybe (Int, Int)
t s = case s =~ "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: (String, String, String, [String]) of
    (_, _, _, [a, b]) -> Just (read a, read b)
    _                 -> Nothing

parseWord :: String -> Instr
parseWord "do()" = Do
parseWord "don't()" = Dont
parseWord x = case t x of
    Just (a, b) -> Mul a b
    Nothing     -> error $ "Invalid input for Mul: " ++ x

day3 :: IO ()
day3 = do
    print (parseWord "mul(123,45)")
    input <- readFile "/Users/alicolver/projects/advent-of-code/calendar/src/03/input.txt"
    let x =  (findMatches "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)" input)
    print x
    let y = map parseWord x
    print (day3_2 y 0 True)

day3' :: [(Int, Int)] -> Int
day3' x = sum (map (\(a, b) -> a * b) x)

day3_2 :: [Instr] -> Int -> Bool -> Int
day3_2 [] x _ = x 
day3_2 ((Mul a b):is) x c = day3_2 is (if c then x + (a * b) else x) c
day3_2 (Do:is) x b = day3_2 is x True
day3_2 (Dont:is) x b = day3_2 is x False