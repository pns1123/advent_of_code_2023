module Lib
    ( printSol
    ) where

import           Control.Monad.State.Lazy
import           Data.List                (transpose)
import           Data.List.Split          (splitOn, splitOneOf)
import qualified Data.Map.Strict          as M

printSol :: IO ()
printSol = do
    content <- readFile "inputs/input"
    print $ (count . solveQ1) content
    print $ solveQ2 content

solveQ1 :: String -> [String]
solveQ1 content = transpose [merge (map roll xs) ys s | ((xs, ys), s) <- zip (map parse ls) ls]
                  where ls = transpose $ lines content

parse :: String -> ([String], [String])
parse l = (filter (/="") $ splitOn "#" l, filter (/="") $ splitOneOf ".O" l)

roll :: String -> String
roll xs = replicate nStones 'O' ++ replicate nSpaces '.'
        where nStones = (length . filter (=='O')) xs
              nSpaces = length xs - nStones

fall :: String -> String
fall xs = replicate nSpaces '.' ++ replicate nStones 'O'
        where nStones = (length . filter (=='O')) xs
              nSpaces = length xs - nStones

merge :: [String] -> [String] -> String -> String
merge xs ys s | length xs == length ys + 1 = head xs ++ combine ys (tail xs)
              | length xs + 1 == length ys = head ys ++ combine xs (tail ys)
              | length xs == length ys     = if head s == '#' then combine ys xs else combine xs ys
              | otherwise                  = error (errorMsg (length xs) (length ys))

combine :: [String] -> [String] -> String
combine xs ys = concat $ zipWith (++) xs ys

errorMsg :: Int -> Int -> String
errorMsg l1 l2 = "incompatible lengths: " ++ show l1 ++ ", " ++ show l2

count :: [String] -> Int
count xs = sum [k * length (filter (=='O') x) | (k,x) <- zip [1..] (reverse xs)]

--Q2
--
solveQ2 :: String -> Int
solveQ2 content = count $ iteratePeriodic tiltCycle (lines content) 1000000000


tilt :: (String -> String) -> ([String] -> [String]) -> [String] -> [String]
tilt f t zs = t [merge (map f xs) ys s | ((xs, ys), s) <- zip (map parse ls) ls]
             where ls = t zs

tiltNorth :: [String] -> [String]
tiltNorth = tilt roll transpose

tiltWest :: [String] -> [String]
tiltWest = tilt roll id

tiltSouth :: [String] -> [String]
tiltSouth = tilt fall transpose

tiltEast :: [String] -> [String]
tiltEast = tilt fall id

tiltCycle :: [String] -> [String]
tiltCycle = tiltEast . tiltSouth . tiltWest . tiltNorth

findPeriod :: (Ord a) => M.Map a Int -> Int -> (a -> a) -> a -> (Int, Int)
findPeriod m c f x = case M.lookup y m of
                     Just k  -> (k, c-k)
                     Nothing -> findPeriod (M.insert y c m) (c+1) f y
                     where y = f x

iteratePeriodic :: (Ord a) => (a -> a) -> a -> Int -> a
iteratePeriodic f iV nIter = compose (replicate k f) iV
                           where (offset, period) = findPeriod M.empty 0 f iV
                                 k = offset + ((nIter - offset) `mod` period)

compose :: [a -> a] -> a -> a
compose = execState . mapM modify
