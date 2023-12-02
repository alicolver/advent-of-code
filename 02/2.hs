import Utils.StringUtils

main :: IO ()
main = do
    input <- readFile "02/input.txt"
    let splitInput = lines input
    print (sum(map getDayVal splitInput))

getDayVal :: String -> Int
getDayVal x
    | isValidGame (splitUp !! 1) = getGameId (head splitUp)
    | otherwise = 0
    where splitUp = splitStringOn (':'==) x

isValidGame :: String -> Bool
isValidGame bagPulls = all ((==True) . isValidPull) (splitStringOn (';'==) bagPulls)

isValidPull :: String -> Bool
isValidPull x = all ((==True) . isValidColor) (splitStringOn (','==) x)

isValidColor :: String -> Bool
isValidColor x = isWithinLimit (read (head numColor) :: Int) (numColor !! 1) 
    where numColor = splitStringOn (' '==) x

isWithinLimit :: Int -> String -> Bool
isWithinLimit number colour = number <= getLimForColor colour

getLimForColor :: String -> Int
getLimForColor "red" = 12
getLimForColor "blue" = 14
getLimForColor "green" = 13
getLimForColor x = 0

getGameId :: String -> Int
getGameId idString = id
    where id = read (splitStringOn (' '==) idString !! 1) :: Int
