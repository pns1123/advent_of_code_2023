module Lib
    ( printSol
    ) where

import           Data.List (transpose)

printSol :: IO ()
printSol = do
     content <- readFile "inputs/input"

     print $ solveQ1 content
     print $ solveQ2 1000000 content

solveQ1 :: String -> Int
solveQ1 content = sum [pairDistance x y
                  |(x,y) <- allPairs . galaxyPositions . expand $ lines content]

solveQ2 :: Int -> String -> Int
solveQ2 f content = sum [expandedPairDistance grid f x y
                    | (x,y) <- (allPairs . galaxyPositions) grid]
                  where grid = lines content

expandRows :: [String] -> [String]
expandRows [] = []
expandRows (x:xs) | all (=='.') x = x:x:expandRows xs
                  | otherwise = x:expandRows xs

expandCols :: [String] -> [String]
expandCols = transpose . expandRows . transpose

expand :: [String] -> [String]
expand = expandCols . expandRows

galaxyPositions :: [String] -> [(Int, Int)]
galaxyPositions css = [(row, col) | (row, cs) <- zip [0..] css,
                                    (col, c) <- zip [0..] cs,
                                    c=='#']

allPairs :: [a] -> [(a,a)]
allPairs xs = [(x,y) | (i,x) <- zip [0..] xs, y <- take i xs]

pairDistance :: (Int, Int) -> (Int, Int) -> Int
pairDistance (x1,y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

emptyRows :: [String] -> Int -> Int -> [Int]
emptyRows xs a b = [i | (i, x) <- zip [0..] xs, i >= a, i < b, all (=='.') x]

countEmpty :: [String] -> (Int,Int) -> (Int,Int) -> Int
countEmpty xs (x1, y1) (x2, y2) = nEmpRows + nEmpCols
                                where top = min x1 x2
                                      bottom = max x1 x2
                                      left = min y1 y2
                                      right = max y1 y2
                                      nEmpRows = length $ emptyRows xs top bottom
                                      nEmpCols = length $ emptyRows (transpose xs) left right

expandedPairDistance :: [String] -> Int -> (Int, Int) -> (Int, Int) -> Int
expandedPairDistance xs f x y = pairDistance x y + (f-1) * countEmpty xs x y

