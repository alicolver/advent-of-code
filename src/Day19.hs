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
    let res = uncurry possibilities tds Map.empty
    print $ length res
    print $ sum res

possibilities :: [Towel] -> [Design] -> Map.Map Design Int -> [Int]
possibilities _ [] _ = []
possibilities s (d:ds) state = snd updated : possibilities s ds (fst updated)
    where
        updated = possibilities' state s [head d] (tail d)

possibilities' :: Map.Map Design Int -> [Towel] -> Design -> Design -> (Map.Map Design Int, Int)
possibilities' m s toCheck []
    | toCheck `elem` s = (Map.insert toCheck 1 m, 1)
    | otherwise = (Map.insert toCheck 0 m, 0)
possibilities' m s toCheck rest
    | Map.member (toCheck++rest) m = (m, m Map.! (toCheck++rest))
    | toCheck `elem` s = (Map.insert (toCheck ++ rest) (r0+r1) m1, r0 +r1)
    | otherwise = (Map.insert (toCheck ++ rest) (r0) m0, r0)
    where
        (m0, r0) = possibilities' m s (toCheck ++ [head rest]) (tail rest)
        (m1, r1) = possibilities' m0 s [head rest] (tail rest)

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