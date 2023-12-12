import Text.Parsec.String
import Text.Parsec
import Data.Char (digitToInt)
import Utils.Utils (frequencyMap)
import qualified Data.Map as Map
import qualified Data.Ord
import Data.List (sort)

data Hand = Hand {
    cards :: String,
    bid :: Int
} deriving (Show, Eq)

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare h1 h2 =
        if val1 /= val2
        then compare val1 val2
        else handCharWise cards1 cards2
        where
            val1 = getHandRank cards1
            val2 = getHandRank cards2
            cards1 = cards h1
            cards2 = cards h2

main :: IO ()
main = do
    input <- readFile "07/input.txt"
    let parsedInput = map parseInput (lines input)
    let sortedInput = quicksort parsedInput
    print (sortedInput)
    print (calculate sortedInput)

calculate :: [Hand] -> Int
calculate xs = sum (zipWith (*) (map bid xs) (enumFromTo 1 (length xs)))

getHandRank :: String -> Int
getHandRank x = calcHand (frequencyMap x)

calcHand :: Map.Map Char Int -> Int
calcHand map = calcHand2 (sortedFrequencies map) numJacks
    where
        numJacks = Map.findWithDefault 0 'J' map

calcHand' :: [Int] -> Int
calcHand' [5] = 17
calcHand' [4, 1] = 6
calcHand' [3, 2] = 5
calcHand' [3, 1, 1] = 4
calcHand' [2, 2, 1] = 3
calcHand' (x:xs) = x

calcHand2 :: [Int] -> Int -> Int
calcHand2 [5] _ = 7
calcHand2  [4, 1] jc = if jc == 1 || jc == 4 then 7 else 6
calcHand2  [3, 2] jc = if jc == 2 || jc == 3 then 7 else 5
calcHand2  [3, 1, 1] jc = if jc == 3 || jc == 1 then 6 else 4
calcHand2  [2, 2, 1] jc
  | jc == 2 = 6
  | jc == 1 = 5
  | otherwise = 3
calcHand2 [2, 1, 1, 1] jc = if jc == 1 || jc == 2 then 4 else 2
calcHand2 [1, 1, 1, 1, 1] jc = if jc == 1 then 2 else 1

handCharWise :: String -> String -> Ordering
handCharWise [] [] = EQ
handCharWise (c1:h1) (c2:h2)
  | cv1 == cv2 = handCharWise h1 h2
  | otherwise = compare cv1 cv2
  where
      cv1 = cardVal c1
      cv2 = cardVal c2

sortedFrequencies :: Map.Map k Int -> [Int]
sortedFrequencies m = reverse (sort (Map.elems m))

cardVal :: Char -> Int
cardVal 'K' = 13
cardVal 'Q' = 12
cardVal 'J' = 1
cardVal 'T' = 10
cardVal 'A' = 14
cardVal x = digitToInt x

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
  let smaller = quicksort [x | x <- xs, x <= pivot]
      larger  = quicksort [x | x <- xs, x > pivot]
  in smaller ++ [pivot] ++ larger

parseInput :: String -> Hand
parseInput input = case parse parseHand "" input of
    Left err -> error (show err)
    Right result -> result

parseHand :: Parser Hand
parseHand = Hand
    <$> (many1 alphaNum <* space)
    <*> integer

integer :: Parser Int
integer = read <$> many1 digit