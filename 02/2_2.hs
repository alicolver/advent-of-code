import Data.List
import Utils.StringUtils

main :: IO ()
main = do
    input <- readFile "02/input.txt"
    let splitInput = lines input
    print (sum(map getDayVal splitInput))

getDayVal :: String -> Int
getDayVal x = getPower (map (replace ';' ',') (splitUp !! 1))
    where splitUp = splitStringOn (':'==) x

getPower :: String -> Int
getPower allPulls = get individualHands "blue" * get individualHands "green" * get individualHands "red"
    where individualHands = splitStringOn (','==) allPulls

get :: [String] -> String -> Int
get hand color = maximum(map getColorFreq (filter (containsWord color) hand))

getColorFreq :: String -> Int
getColorFreq x = read (head numColor) :: Int
    where numColor = splitStringOn (' '==) x
