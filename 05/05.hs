import Text.Parsec
import qualified Data.Functor.Identity
import Text.Parsec.String
import Utils.Utils (flatmap)
import Data.List
import Data.Function (on)

data Seed = Seed {
    seedStart :: Int,
    seedRange :: Int
}

data Mapping = Mapping {
    destStart :: Int,
    sourceStart :: Int,
    range :: Int
} deriving (Show)

data MappingHolder = MappingHolder {
    from :: String,
    to :: String,
    mappings :: [Mapping]
} deriving (Show)

main :: IO ()
main = do
    inputSeeds <- readFile "05/inputSeeds.txt"
    input <- readFile "05/input.txt"
    let seeds = parseSeeds inputSeeds
    let mappings = parseAllMappings input
    print seeds
    print (parseAllMappings input)
    let results = map (calculate mappings) seeds
    print (minimum results)
    let reversedMappings = reverse mappings
    print reversedMappings
    let orderedSeeds = sortBy (flip compare `on` snd) (parseSeeds2 inputSeeds)
    print orderedSeeds

calculate :: [MappingHolder] -> Int -> Int
calculate [] x = x
calculate (mh:ms) val = calculate ms (getMappingValue (mappings mh) val)

getMappingValue :: [Mapping] -> Int -> Int
getMappingValue [] x = x
getMappingValue (m:ms) val = if ((sourceStart m + range m) > val) && sourceStart m < val
    then (val - sourceStart m) + destStart m
    else getMappingValue ms val

createAllSeeds :: [(Int,Int)] -> [Int]
createAllSeeds [] = []
createAllSeeds ((start,range):rest) = create' start range 0 ++ createAllSeeds rest

create' :: Int -> Int -> Int -> [Int]
create' _ 0 _ = []
create' start range count = start + count : create' start (range - 1) (count + 1)

parseSeeds :: String -> [Int]
parseSeeds seeds = case parse holder "" seeds of
    Left err -> error (show err)
    Right value -> value
  where
    holder = do
        string "seeds: "
        sepBy number space

parseSeeds2 :: String -> [(Int,Int)]
parseSeeds2 seeds = case parse pairs "" seeds of
    Left err -> error (show err)
    Right value -> value
  where
    pairs = do
        string "seeds:"
        spaces
        endBy parseIntPair spaces

parseIntPair :: Parser (Int, Int)
parseIntPair = (,) <$> number <*> (spaces *> number)

parseAllMappings :: String -> [MappingHolder]
parseAllMappings mappings = case parse holder "" mappings of
    Left err -> error (show err)
    Right value -> value
  where
    holder = sepEndBy parseAllMapping newline

parseAllMapping :: Parser MappingHolder
parseAllMapping = do
     MappingHolder
        <$> (many1 alphaNum <* string "-to-")
        <*> (many1 alphaNum <* string " map:\n")
        <*> sepEndBy parseMapping newline

parseMapping :: Parser Mapping
parseMapping = do
    Mapping
        <$> number
        <*> (space *> number <* char ' ')
        <*> number

number :: ParsecT String u Data.Functor.Identity.Identity Int
number = read <$> many1 digit