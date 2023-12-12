module Lib
    ( printSol
    ) where

import           Control.Monad (guard)
import           Data.Text     (pack, splitOn, unpack)

printSol :: IO ()
printSol = do
    content <- readFile "inputs/test_input"
    let preproc = [(s, getCounts c) | [s,c] <- words <$> lines content]

    print $ [ length $ filter (validRecord $ getCounts c) $ solve (getCounts c) s
            | [s,c] <- words <$> lines content]

solve :: [Int] -> String -> [String]
solve counts xs = do
    b <- branch xs
    guard (length (filter (=='#') b) <= sum counts)
    if '?' `notElem` b then return b else solve counts b

getCounts :: String -> [Int]
getCounts xs = read . unpack <$> splitOn (pack ",") (pack xs)

validRecord :: [Int] -> String -> Bool
validRecord ref xs = ref == [(length . unpack) x
                            | x <- splitOn (pack ".") (pack xs), x /= pack ""]

branch :: String -> [String]
branch = _branch []

_branch :: String -> String -> [String]
_branch prefix [] = [prefix]
_branch prefix (x:xs) | x == '?'  = [prefix ++ '.' : xs, prefix ++ '#' : xs]
                      | otherwise = _branch (prefix ++ [x]) xs
