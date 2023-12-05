module Lib
    ( printSol
    ) where

import           Control.Monad.State
import           Data.List           (sortBy)
import           Data.List.Split     (splitOn)

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    print $ solveQ2 content

--Q1
--

solveQ1 :: String -> Int
solveQ1 s = minimum $ map location seeds
            where seeds = parseSeeds s
                  maps = parseMaps s
                  location = compose [(evalMap . buildMap) m | m <- maps]

parseSeeds :: String -> [Int]
parseSeeds s = read <$> (words . drop 7 . head . lines) s

parseMaps :: String -> [[[Int]]]
parseMaps s = map (map read . words) <$> drop 1 <$> lines <$> (drop 1 . splitOn "\n\n") s


buildMap :: [[Int]] -> [(Int, Int, Int)]
buildMap xss = [(xs !! 1, xs !! 1 + xs !! 2, xs !! 0 - xs !! 1) | xs <- xss]

evalMap :: [(Int, Int, Int)] -> (Int -> Int)
evalMap xs k = case filter (\(a, b, _) -> a <= k && k < b) xs of
                    []          -> k
                    ((_,_,d):_) -> k + d

compose :: [a -> a] -> a -> a
compose = execState . mapM modify


--Q2
--
solveQ2 :: String -> Int
solveQ2 content = minimum $ fst <$> iterateBounds maps seeds
          where seeds = sortBy compareIntervals $ seedRanges $ parseSeeds content
                maps = sortBy compareTriple . buildMap <$> parseMaps content
                compareTriple (_,a,_) (_,b,_) = compare a b

iterateBounds :: [[(Int, Int, Int)]] -> [(Int, Int)] -> [(Int, Int)]
iterateBounds [] acc = acc
--iterateBounds (m:ms) acc = iterateBounds ms $ acc >>= update m
iterateBounds ms acc = foldl (\a m -> a >>= update m) acc ms

seedRanges :: [Int] -> [(Int,Int)]
seedRanges []       = []
seedRanges (s:l:xs) = (s, s+l-1) : seedRanges xs
seedRanges _        = error "uneven length of seed input"

compareIntervals :: (Int, Int) -> (Int, Int) -> Ordering
compareIntervals (a,_) (b,_) = compare a b

update :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
update rs x = case filter (not . disjoint x) rs of
                     []          -> [x]
                     overlapping -> _divide x overlapping

disjoint :: (Int, Int) -> (Int, Int, Int) -> Bool
disjoint (a,b) (c,d,_) = b <= c || d < a

_divide :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
_divide _ []               = []
_divide (a,b) [(l,u,d)]    = divide (a, b) (l,u,d)
_divide (a,b) ((l,u,d):rs) = divide (a, min b u) (l,u,d) ++ _divide (u,b) rs

divide :: (Int, Int) -> (Int, Int, Int) -> [(Int, Int)]
divide (a,b) (l, u, d) | l <= a && b <= u  = [(a+d,b+d)]                  -- subset
                       | a <= l && u <  b  = [(a, l-1),(l+d,u+d),(u,b)]   -- supset
                       | b <  l || u <= a  = [(a,b)]                      -- disjoint
                       | a <= l            = [(a+d, l+d),(a+(u-l),b)]     -- left ovlp
                       | u <= b            = [(a+d, u+d), (u,b)]          -- right ovlp



--minDisjoint :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
--minDisjoint [] ys     = reverse ys
--minDisjoint (x:xs) ys = minDisjoint xs (insert x ys)
--
-- assumes ys is sorted w/ descending start points
-- assumes start point of x is >= max start point of ys
--insert :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
--insert x []     = [x]
--insert x (y:ys) = (shatter y x) ++ ys
--
--shatter :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
--shatter (a,b) (c,d) | d <= b    = [(d,b), (c,d), (a,c)] -- subset
--                    | c <= b    = [(b,d), (c,b), (a,c)] -- overlap
--                    | otherwise = [(c,d), (a,b)]        -- disjoint
--
