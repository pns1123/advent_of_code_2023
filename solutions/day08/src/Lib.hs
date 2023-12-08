module Lib
    ( printSol
    ) where

import qualified Data.Map as M

printSol :: String -> IO ()
printSol inputFilePath = do
     content <- readFile inputFilePath
     print $ solveQ1 content
     print $ solveQ2 content


data Move = L | R deriving (Read, Show)

solveQ1 :: String -> Int
solveQ1 content = navigate (=="ZZZ") directions (cycle moves) "AAA" 1
                  where moves = getMoveSequence content
                        directions = getDirections $ (drop 2 . lines) content

solveQ2 :: String -> Int
solveQ2 content = foldr lcm 1 [navigate terminalState directions (cycle moves) i 1
                              | i <- initial]
                  where moves = getMoveSequence content
                        directions = getDirections $ (drop 2 . lines) content
                        initial = getInitialStates $ lines content


getMoveSequence :: String -> [Move]
getMoveSequence content = [read [x] | x <- (head . lines) content]

getDirections :: [String] -> M.Map String (String, String)
getDirections xs = M.fromList [(getKey x, (getLeftVal x, getRightVal x)) | x <- xs]
                   where getKey = take 3
                         getLeftVal = take 3 . drop 7
                         getRightVal = take 3 . drop 12


nextState :: M.Map String (String, String) -> String -> Move -> String
nextState m s R = snd (m M.! s)
nextState m s L = fst (m M.! s)


navigate :: (String -> Bool) -> M.Map String (String, String) -> [Move] -> String -> Int -> Int
navigate p d (m:ms) s k | p next    = k
                        | otherwise = navigate p d ms next (k+1)
                        where next = nextState d s m

--Q2
--
initialState :: String -> Bool
initialState s = last s == 'A'

getInitialStates :: [String] -> [String]
getInitialStates xs = filter initialState $ map (take 3) $ drop 2 xs

terminalState :: String -> Bool
terminalState s = last s == 'Z'
