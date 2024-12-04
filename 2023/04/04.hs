import Data.List
import qualified Data.Map as Map
import Text.Parsec (parse, many1, char, string, spaces, digit, between)

data Card = Card {
    cardNumber :: Int,
    winning :: [Int],
    chosen :: [Int]
} deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- readFile "04/input.txt"
    let splitInput = lines input
    let parsedCards = map parseCard splitInput
    let scores = map getScoreForGame parsedCards
    print (sum scores)
    let counted = countCard (zip parsedCards (replicate (length parsedCards) 1)) (map getNumMatches parsedCards)
    print (sum (map snd counted))

countCard :: [(Card, Int)] -> [Int] -> [(Card,Int)]
countCard _ [] = []
countCard ((card,freq):cards) (score:scores) = (card,freq) : countCard (countCards' cards freq score) scores

countCards' :: [(Card, Int)] -> Int -> Int -> [(Card, Int)]
countCards' cards _ 0 = cards
countCards' ((card,freq):cards) prevFreq score =  (card, freq + prevFreq) : countCards' cards prevFreq (score - 1)

getScoreForGame :: Card -> Int
getScoreForGame card
    | numMatches == 0 = 0
    | otherwise = 2 ^ (numMatches - 1)
    where numMatches = getNumMatches card

getNumMatches :: Card -> Int
getNumMatches card = length (winning card `intersect` chosen card)

parseCard :: String -> Card
parseCard cardString = case parse card "" cardString of
    Left err -> error (show err)
    Right value -> value
  where
    card = Card
        <$> (prefix *> number)
        <*> (char ':' *> numList <* char '|')
        <*> numList
    prefix = string "Card" *> spaces
    number = read <$> many1 digit
    numList = many1 (between spaces spaces number)