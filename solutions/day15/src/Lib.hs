module Lib
    ( printSol
    ) where

import           Data.Char       (ord)
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

printSol :: IO ()
printSol = do
    content <- readFile "inputs/input"
    print $ solveQ1 content
    print $ solveQ2 content

solveQ1 :: String -> Int
solveQ1 = sum . map hash . preproc

solveQ2 :: String -> Int
solveQ2 = countLengths . foldl updateBoxes (M.fromList [(k, []) | k <- [0..255]]) . preproc

preproc :: String -> [String]
preproc = splitOn ","  . filter (/='\n')

hash :: String -> Int
hash = foldl (\acc x -> (17 * (acc + ord x)) `mod` 256) 0

updateBoxes :: M.Map Int [(String, Int)] -> String -> M.Map Int [(String, Int)]
updateBoxes m s | last s == '-' = delete m s
                | otherwise     = add m s

delete :: M.Map Int [(String, Int)] -> String -> M.Map Int [(String, Int)]
delete m s = M.adjust (filter (\(y,_) -> y/=x)) (hash x) m
             where x = init s

add :: M.Map Int [(String, Int)] -> String -> M.Map Int [(String, Int)]
add m s = M.adjust (replaceOrAppend (x, read [last s])) h m
        where x = (init . init) s
              h = hash x

replaceOrAppend :: (String, Int) -> [(String, Int)] -> [(String, Int)]
replaceOrAppend (k,v) xs = if k `elem`(fst <$> xs) then replace xs (k,v) else xs ++ [(k,v)]


replace :: [(String,Int)] -> (String, Int) -> [(String, Int)]
replace xs x = map (\y -> if fst y== fst x then x else y) xs

countLengths :: M.Map Int [(String, Int)] -> Int
countLengths = M.foldrWithKey f 0
             where f k x acc = (k+1) * sum (zipWith (*) [1..] (snd <$> x)) + acc
