module Day17(
    day17
) where
import Text.Parsec
import Text.Parsec.String
import Lib (intParser)
import Data.Bits (xor)

type Command = (Int,Int)

data Registers = Registers {
    a :: Int,
    b :: Int,
    c :: Int
} deriving (Show)

data Program = Program {
    registers :: Registers,
    commands :: [Int]
} deriving (Show)

data ProgState = ProgState {
    state :: Registers,
    instPointer :: Int,
    output :: [Int]
} deriving (Show)

handleCommand :: ProgState -> Command -> ProgState
handleCommand (ProgState r i o) (0,literal) =
    ProgState (Registers (a r `div` (2 ^ getComboValue r literal)) (b r) (c r)) (i + 2) o

handleCommand (ProgState r i o) (1,literal) =
    ProgState (Registers (a r) (b r `xor` literal) (c r)) (i+2) o

handleCommand (ProgState r i o) (2,literal) =
    ProgState (Registers (a r) (getComboValue r literal `mod` 8) (c r)) (i+2) o

handleCommand (ProgState r i o) (3,literal)
    | a r == 0 = ProgState r (i+2) o
    | otherwise = ProgState r literal o

handleCommand (ProgState r i o) (4,_) =
    ProgState (Registers (a r) (b r `xor` c r) (c r)) (i+2) o

handleCommand (ProgState r i o) (5,literal) =
    ProgState r (i+2) (o ++ [getComboValue r literal `mod` 8])

handleCommand (ProgState r i o) (6,literal) =
    ProgState (Registers (a r) (a r `div` (2 ^ getComboValue r literal)) (c r)) (i + 2) o

handleCommand (ProgState r i o) (7,literal) =
    ProgState (Registers (a r) (b r) (a r `div` (2 ^ getComboValue r literal))) (i + 2) o

handleCommand _ _ = error "Borked!"

getComboValue :: Registers -> Int -> Int
getComboValue rs 4 = a rs
getComboValue rs 5 = b rs
getComboValue rs 6 = c rs
getComboValue _ x = x

runProgram :: ProgState -> [Int] -> [Int]
runProgram ps commands
    | instPointer ps + 1 >= length commands = output ps
    | otherwise = runProgram handledCommand commands
    where
        command = getCommand (instPointer ps) commands
        handledCommand = handleCommand ps command

search :: [Int] -> [Int] -> Int
search program target = loop a program target
    where
        a = if length target == 1 then 0 else 8 * search program (tail target)

loop :: Int -> [Int] -> [Int] -> Int
loop a program target
    | runProgram ps program == target = a
    | otherwise = loop (a+1) program target
    where
        ps = ProgState (Registers a 0 0) 0 []

getCommand :: Int -> [Int] -> (Int,Int)
getCommand ip coms = (coms !! ip, coms !! (ip+1))

-- parsing and orchestration

day17 :: IO ()
day17 = do
    input <- readFile "src/input/17.txt"
    let program = parseInput input
    print $ runProgram (ProgState (registers program) 0 []) (commands program)
    print $ search (commands program) (commands program)

registerParser :: String -> Parser Int
registerParser s = do
    _ <- string $ "Register " ++ s ++ ": "
    v <- intParser
    _ <- newline
    return v

registersParser :: Parser Registers
registersParser = do
    pa <- registerParser "A"
    pb <- registerParser "B"
    pc <- registerParser "C"
    return (Registers pa pb pc)

parseProgram :: Parser Program
parseProgram = do
    rs <- registersParser
    _ <- many1 newline
    _ <- string "Program: "
    Program rs <$> parseNums

parseInput :: String -> Program
parseInput input = case parse parseProgram "" input of
    Left err -> error (show err)
    Right result -> result

parseNums :: Parser [Int]
parseNums = intParser `sepBy` char ','