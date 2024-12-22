module Day19(
    day19
) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Parsing (parseCsv)
import qualified Data.Map as Map

type Towel = String
type Design = String

day19 :: IO ()
day19 = do
    input <- readFile "src/input/19.txt"
    let tds = parseInput input
    print tds
    print $ sum (map (isPossible (fst tds)) (snd tds))

isPossible :: [String] -> String -> Int
isPossible s design = isPossible' (Map.empty) s [head design] (tail design)

isPossible' :: (Map.Map String Int) -> [String] -> String -> String -> Int
isPossible' m s toCheck [] = if toCheck `elem` s then 1 else 0
isPossible' m s toCheck rest
    | toCheck `elem` s = isPossible'' m s [head rest] (tail rest) + isPossible'' m s (toCheck ++ [head rest]) (tail rest)
    | otherwise = isPossible'' m s (toCheck ++ [head rest]) (tail rest)
    where 
        isPossible'' = do
            v <- gets $ Map.lookup toCheck
            case v of
                Just res -> return res
                Nothing -> return isPossible' m 


parseTowelDesigns :: Parser ([Towel], [Design])
parseTowelDesigns = do
    ts <- parseCsv
    _ <- many1 newline
    ds <- many (noneOf "\n") `sepBy` newline
    return (ts, ds)

parseInput :: String -> ([Towel], [Design])
parseInput input = case parse parseTowelDesigns "" input of
    Left err -> error (show err)
    Right result -> result