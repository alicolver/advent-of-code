import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map 
import Data.List (cycle)

data Node = Node {
    name :: String,
    left :: String, 
    right :: String
} deriving Show

main :: IO ()
main = do
    inputSteps <- readFile "08/inputSteps.txt"
    input <- readFile "08/input.txt"
    let processed = map parseInput (lines input)
    let generatedMap = generateMap processed
    print (calc (cycle inputSteps) generatedMap 0 (allZs) (generatedMap Map.! "AAA"))
    let startingNodes = filter (doesNodeEndWith 'A') processed 
    let part2res = (map (calc (cycle inputSteps) generatedMap 0 (doesNodeEndWith 'Z')) startingNodes) 
    print (foldl1 lcm part2res)

getNewNode :: Char -> Map.Map String Node -> Node -> Node 
getNewNode s m n = if s == 'L' then m Map.! (left n) else m Map.! (right n)

doesNodeEndWith :: Char -> Node -> Bool 
doesNodeEndWith c n = last (name n) == c

allZs :: Node -> Bool 
allZs n = name n == "ZZZ"

calc :: String -> Map.Map String Node -> Int -> (Node -> Bool) -> Node -> Int
calc (s:ss) m c f n = if (f n) then c else calc ss m (c + 1) f (getNewNode s m n)

generateMap :: [Node] -> Map.Map String Node
generateMap nodes = Map.fromList (zip (map name nodes) nodes)

parseInput :: String -> Node
parseInput input = case parse parseNode "" input of
    Left err -> error (show err)
    Right result -> result

parseNode :: Parser Node 
parseNode = Node 
    <$> (many1 alphaNum <* string " = (")
    <*> (many1 alphaNum <* string ", ")
    <*> (many1 alphaNum <* string ")")