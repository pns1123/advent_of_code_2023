module Lib
    ( printSol
    ) where

import qualified Data.Char as C
import qualified Data.Text as T

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    print $ solveQ2 content

-- Q1
solveQ1 :: String -> Int
solveQ1 c = sum $ map (read . concatFirstLast . filter C.isDigit) $ lines c

concatFirstLast :: String -> String
concatFirstLast s = [head s, last s]

-- Q2
solveQ2 :: String -> Int
solveQ2 c = sum $ map (read . concatFirstLast . reverse . digitSubstring "") $ lines c

data DigitLiteral = One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Bounded, Enum, Read)

instance Show DigitLiteral where
    show One = "one"
    show Two = "two"
    show Three = "three"
    show Four = "four"
    show Five = "five"
    show Six = "six"
    show Seven = "seven"
    show Eight = "eight"
    show Nine = "nine"

literalChar :: DigitLiteral -> Char
literalChar One = '1'
literalChar Two = '2'
literalChar Three = '3'
literalChar Four = '4'
literalChar Five = '5'
literalChar Six = '6'
literalChar Seven = '7'
literalChar Eight = '8'
literalChar Nine = '9'

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

isDigitLiteralPrefix :: T.Text -> DigitLiteral -> Bool
isDigitLiteralPrefix xs d = T.isPrefixOf (T.pack $ show d) xs

findLiteralPrefix :: T.Text -> Maybe DigitLiteral
findLiteralPrefix xs = safeHead $ dropWhile (not . isDigitLiteralPrefix xs) [One .. Nine]

digitSubstring :: String -> String -> String
digitSubstring ys [] = ys
digitSubstring ys xs = case findLiteralPrefix (T.pack xs) of
                            Nothing -> charSubstring xs ys
                            Just x  -> digitSubstring ((literalChar x):ys) $ tail xs

charSubstring :: String -> String -> String
charSubstring [] ys = ys
charSubstring (x:xs) ys = if C.isDigit x then digitSubstring (x:ys) xs else digitSubstring ys xs
