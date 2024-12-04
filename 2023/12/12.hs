import Text.Parsec.String
import Text.Parsec

data Row = Row {
    ts :: String,
    c :: [Int]
} deriving (Show)

main :: IO ()
main = do 
    input <- readFile "12/input.txt"
    let rows = map pI (lines input)
    print rows 

pI :: String -> Row
pI input = case parse pRow "" input of
    Left err -> error (show err)
    Right result -> result

pRow :: Parser Row 
pRow = Row 
    <$> manyTill anyChar (try space)
    <*> sepBy (read <$> many1 digit) (char ',') 