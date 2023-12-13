module Lib
    ( printSol
    ) where

import           Data.List       (transpose)
import           Data.List.Split (splitOn)

printSol :: IO ()
printSol = do
    content <- readFile "inputs/input"
    let inputs =  splitOn [""] (lines content)

    print $ sum $ map solveQ1 inputs
    print $ sum $ map solveQ2 inputs

isSymmetric :: (Eq a) => [a] -> [a] -> Bool
isSymmetric l r = and $ zipWith (==) (reverse l) r

findReflectionCenter :: (Eq a) => Int -> [a] -> Maybe Int
findReflectionCenter k xs = case splitAt k xs of
                            (_, []) -> Nothing
                            (l, r) -> if isSymmetric l r
                                      then Just k
                                      else findReflectionCenter (k+1) xs

solveQ1 :: [String] -> Int
solveQ1 xs = case findReflectionCenter 1 xs of
             Just k -> 100 * k
             Nothing -> case findReflectionCenter 1 (transpose xs) of
                        Just j  -> j
                        Nothing -> error "no symmetry axis found"

isAlmostSymmetric :: (Eq a) => [[a]] -> [[a]] -> Bool
isAlmostSymmetric l r = sum (zipWith countDifferences (reverse l) r) == 1

countDifferences :: (Eq a) => [a] -> [a] -> Int
countDifferences xs ys = sum [1 | (x,y) <- zip xs ys, x/=y]

findBudgedReflectionCenter :: (Eq a) => Int -> [[a]] -> Maybe Int
findBudgedReflectionCenter k xs = case splitAt k xs of
                                  (_, []) -> Nothing
                                  (l, r) -> if isAlmostSymmetric l r
                                      then Just k
                                      else findBudgedReflectionCenter (k+1) xs

solveQ2 :: [String] -> Int
solveQ2 xs = case findBudgedReflectionCenter 1 xs of
             Just k -> 100 * k
             Nothing -> case findBudgedReflectionCenter 1 (transpose xs) of
                        Just j  -> j
                        Nothing -> error "no budged symmetry axis found"
