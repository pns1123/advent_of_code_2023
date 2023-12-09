module Lib
    ( printSol
    ) where

printSol :: String -> IO ()
printSol inputPath = do
    content <- readFile inputPath
    print $ solveQ1 content
    print $ solveQ2 content

solveQ1 :: String -> Int
solveQ1 content = sum $ iterateSeq <$> sequences
                  where sequences = getSequences content

solveQ2 :: String -> Int
solveQ2 content = sum $ iterateSeqStart <$> sequences
                  where sequences = getSequences content

getSequences :: String -> [[Int]]
getSequences content = map read . words <$> lines content

diffSeq :: [Int] -> [Int]
diffSeq xs = [a-b | (b,a) <- zip xs (tail xs)]

aggDiffs :: [Int] -> [Int] -> [Int]
aggDiffs xs ys | all (==0) xs = ys
               | otherwise  = aggDiffs d (last d : ys)
               where d = diffSeq xs

iterateSeq :: [Int] -> Int
iterateSeq [] = 0
iterateSeq xs = sum $ aggDiffs xs [last xs]


aggDiffsStart :: [Int] -> [Int] -> [Int]
aggDiffsStart xs ys | all (==0) xs = ys
                    | otherwise  = aggDiffsStart d (head d : ys)
                    where d = diffSeq xs

iterateSeqStart :: [Int] -> Int
iterateSeqStart [] = 0
-- agg = b - x <=> x = b - agg
iterateSeqStart xs = foldl (flip (-)) 0 (aggDiffsStart xs [head xs])
