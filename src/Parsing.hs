module Parsing (
    parseCsv
) where
import Text.Parsec
import Text.Parsec.String
import Data.Char (isSpace)

parseCsv :: Parser [String]
parseCsv = sepBy parseField (char ',' *> spaces)

parseField :: Parser String
parseField = trim <$> many (noneOf ",\n")

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse