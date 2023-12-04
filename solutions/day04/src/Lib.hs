module Lib
    ( printSol
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    print $ solveQ2 content

    --print $ solveQ2 content


--Q1
--
solveQ1 :: String -> Int
solveQ1 content = sum $ score . winningNumbers . parseLine <$> T.lines (T.pack content)

parseLine :: T.Text -> ([Int], [Int])
parseLine = readNumbers . splitWords . splitLists . dropGame

dropGame :: T.Text -> T.Text
dropGame = T.drop 1 . T.dropWhile (/=':')

splitLists :: T.Text -> (T.Text, T.Text)
splitLists t = (prefix, T.drop 1 suffix)
               where (prefix, suffix) = T.break (=='|') t

splitWords :: (T.Text, T.Text) -> ([T.Text], [T.Text])
splitWords (t1, t2) = (T.words t1, T.words t2)

readNumbers :: ([T.Text], [T.Text]) -> ([Int], [Int])
readNumbers (ts1, ts2) = (readText <$> ts1, readText <$> ts2)
                         where readText = read . T.unpack

winningNumbers :: ([Int], [Int]) -> [Int]
winningNumbers (xs, ys) = [y | y <- ys, y `elem` xs]

score :: [Int] -> Int
score []     = 0
score (_:xs) = foldr (\_ acc -> 2*acc) 1 xs


--Q2
--
--
solveQ2 :: String -> Int
solveQ2 content = sum $ accCards _lookup acc
                  where _lookup = addedCards _cards
                        acc = [1 | _ <-  _cards]
                        _cards = cardValues content

cardValues :: String -> [(Int, Int)]
cardValues content  = zip [1..] $ map length $ winningNumbers . parseLine <$> T.lines (T.pack content)

addedCards :: [(Int, Int)] -> [(Int, [Int])]
addedCards xs = [(n, findCards n s xs) | (n,s) <- xs]
                where findCards n s ys = map fst $ take s $ drop n ys


accCards :: [(Int, [Int])] -> [Int] -> [Int]
accCards [] acc = acc
accCards ((curNum, toUpdate):xs) acc = accCards xs [if i `elem` toUpdate
                                                    then acc !! (curNum-1) + a
                                                    else a
                                                    | (i, a) <- zip [1..] acc]
