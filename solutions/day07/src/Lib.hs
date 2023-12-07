module Lib
    ( printSol
    ) where

import qualified Data.List       as L
import qualified Data.Map.Strict as M

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    print $ solveQ2 content

--Q1
--data Label = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Bounded, Enum, Eq, Ord, Read, Show)

--Q2
data Label = J | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | Q | K | A deriving (Bounded, Enum, Eq, Ord, Read, Show)


readLabel :: Char -> Label
readLabel 'A' = A
readLabel 'K' = K
readLabel 'Q' = Q
readLabel 'J' = J
readLabel 'T' = T
readLabel '9' = Nine
readLabel '8' = Eight
readLabel '7' = Seven
readLabel '6' = Six
readLabel '5' = Five
readLabel '4' = Four
readLabel '3' = Three
readLabel '2' = Two
readLabel _   = error "parse error: character unknown"

solveQ1 :: String -> Int
solveQ1 content = sum $ zipWith (*) [1..] $ map snd (sortHands $ parseInput content)

parseInput :: String -> [(String, Int)]
parseInput s = [(x !! 0, read $ x !! 1) | x <- words <$> lines s]

sortHandsTransform :: (String -> String) -> [(String, Int)] -> [(String, Int)]
sortHandsTransform f = L.sortBy (\(s1,_) (s2,_) -> compareHandStrTransform f s1 s2)

sortHands :: [(String, Int)] -> [(String, Int)]
sortHands = sortHandsTransform id

counts :: String -> [(Char, Int)]
counts = sortLabels . M.toList . countLabels

sortLabels :: [(Char, Int)] -> [(Char, Int)]
sortLabels = L.sortBy (\x y -> flip compare (snd x) (snd y))

countLabels :: String -> M.Map Char Int
countLabels s = M.fromListWith (+) [(c, 1) | c <- s]

compareHandStrTransform :: (String -> String) -> String -> String -> Ordering
compareHandStrTransform f s1 s2 = case compareCounts (counts $ f s1) (counts $ f s2) of
                                  GT -> GT
                                  LT -> LT
                                  EQ -> compare (strToLabels s1) (strToLabels s2)

compareHandStr :: String -> String -> Ordering
compareHandStr = compareHandStrTransform id

compareCounts :: [(Char, Int)] -> [(Char, Int)] -> Ordering
compareCounts []     []     = EQ
compareCounts ((_,c1):xs) ((_,c2):ys)   | c1 >  c2 = GT
                                        | c1 <  c2 = LT
                                        | c1 == c2 = compareCounts xs ys
compareCounts _ _ = error "this state should not be reached"

strToLabels :: String -> [Label]
strToLabels s = [readLabel c | c <- s]

--Q2
--

solveQ2 :: String -> Int
solveQ2 content = sum $ zipWith (*) [1..] $ map snd (sortHandsTransform evaluateJoker $ parseInput content)

evaluateJoker :: String -> String
evaluateJoker s = L.maximumBy compareHandStr [map (replaceJoker c) s
                                                 | c <- alphabet]
                  where alphabet =  ['2', '3', '4', '5', '6', '7',
                                     '8', '9', 'T', 'J', 'Q', 'K', 'A']

replaceJoker :: Char -> Char -> Char
replaceJoker x 'J' = x
replaceJoker _  y  = y
