module Day25 (
    day25
) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (transpose, nub)

data LockKey = LockKey {
    lock :: Maybe Heights,
    key :: Maybe Heights
} deriving (Show)
data LocksKeys = LocksKeys {
    locks :: [Heights],
    keys :: [Heights]
} deriving (Show)
type Heights = [Int]

part1 :: LocksKeys -> Int
part1 lks = sum $ map (tryAllKeysInLock (nub (keys lks))) (nub (locks lks))

tryAllKeysInLock :: [Heights] -> Heights -> Int
tryAllKeysInLock keys lock = length $ filter (tryKeyInLock lock) keys

tryKeyInLock :: Heights -> Heights -> Bool
tryKeyInLock lock key = all (\(a,b) -> a+b < 6) (zip lock key)

convertToKeyOrLock :: [String] -> LockKey
convertToKeyOrLock lk
    | isLock = LockKey (Just keyHeights) Nothing
    | otherwise = LockKey Nothing (Just keyHeights)
    where
        isLock = all (== '#') (head lk)
        formatted = if isLock then lk else reverse lk
        removeFirstRow = drop 1 formatted
        transposed = transpose removeFirstRow
        keyHeights = map (length . takeWhile (== '#')) transposed

getAllLockKeys :: [LockKey] -> LocksKeys -> LocksKeys
getAllLockKeys [] lks = lks
getAllLockKeys (lk:lks) allLks = getAllLockKeys lks (aggregateLocks lk allLks)

aggregateLocks :: LockKey -> LocksKeys -> LocksKeys
aggregateLocks (LockKey (Just x) Nothing) (LocksKeys ls ks) = LocksKeys (ls ++ [x]) ks
aggregateLocks (LockKey Nothing (Just x)) (LocksKeys ls ks) = LocksKeys ls (ks ++ [x])
aggregateLocks _ _ = error "Something has gone wrong!"

day25 :: IO ()
day25 = do
    input <- readFile "src/input/25.txt"
    let lks = map convertToKeyOrLock $ parseInput input
    let allLocksAndKeys = getAllLockKeys lks (LocksKeys [] [])
    print $ part1 allLocksAndKeys

blockParser :: Parser [String]
blockParser = count 7 (count 5 anyChar <* newline)

blocksParser :: Parser [[String]]
blocksParser = blockParser `sepBy` many1 newline

parseInput :: String -> [[String]]
parseInput input = case parse blocksParser "" input of
    Left err -> error (show err)
    Right result -> result