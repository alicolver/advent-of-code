import Text.Parsec
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
    let mappingsX = parseAllMappings input
    print seeds
    let results = map (calculate mappingsX) seeds
    print (minimum results)
    let seeds2 = parseSeeds2 inputSeeds
    let seedRanges = map createRange seeds2
    print seedRanges
    let part2res = partTwo seedRanges mappingsX
    print (minimum (map fst part2res))

-- 7873084

partTwo :: [(Int, Int)] -> [MappingHolder] -> [(Int, Int)]
partTwo seeds [] = seeds
partTwo seeds (m:ms) = partTwo ((applyMapping mappingsForM) seeds) ms
    where 
        mappingsForM = mappings m

applyMapping :: [Mapping] -> [(Int, Int)] -> [(Int, Int)]
applyMapping ms seeds = (flatmap (applyMappingToSeedRange ms) seeds)

applyMappingToSeedRange :: [Mapping] -> (Int, Int) -> [(Int, Int)]
applyMappingToSeedRange [] (start, end) = [(start, end)]
applyMappingToSeedRange (mapping:ms) (start, end)  = 
    if isSeedStartInSourceRange || isSeedEndInSourceRange 
    then 
        if isSeedStartInSourceRange && isSeedEndInSourceRange
        then [(start + mapValueToApply, end + mapValueToApply)]
        else if isSeedStartInSourceRange && not isSeedEndInSourceRange
            then [(start + mapValueToApply, dMapEnd), (sMapEnd + 1, end)] -- start to end
            else [(start, sMapStart - 1), (dMapStart, end + mapValueToApply)] -- end overlap 
    else if sMapStart > start  && sMapEnd < end 
        then [ -- total overlap 
            (start, sMapStart - 1),
            (dMapStart, dMapEnd),
            (sMapEnd + 1, end)
        ]
        else applyMappingToSeedRange ms (start, end)
    where 
        isSeedStartInSourceRange = start >= sMapStart && start < sMapEnd
        isSeedEndInSourceRange = end < sMapEnd && end >= sMapStart
        sMapStart = sourceStart mapping
        sMapEnd = sourceStart mapping + range mapping
        dMapStart = destStart mapping
        dMapEnd = destStart mapping + range mapping
        mapValueToApply = dMapStart - sMapStart

--    [1, 2, 3, 4, 5, 6, 7]
--       [2, 3, 4]
-- [0, 1, 2]
--                [5, 6, 7, 8]
-- [0, 1, 2, 3, 4, 5, 6, 7, 8]
-- [0]

createRange :: (Int, Int) -> (Int, Int)
createRange (start, interval) = (start, start + interval)

calculate :: [MappingHolder] -> Int -> Int
calculate [] x = x
calculate (mh:ms) val = calculate ms (getMappingValue (mappings mh) val)

getMappingValue :: [Mapping] -> Int -> Int
getMappingValue [] x = x
getMappingValue (m:ms) val = if ((sourceStart m + range m) > val) && sourceStart m < val
    then (val - sourceStart m) + destStart m
    else getMappingValue ms val

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
        <*> (space *> number <* space)
        <*> number

number :: Parser Int
number = read <$> many1 digit