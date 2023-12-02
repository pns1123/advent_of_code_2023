module Lib
    ( printSol
    ) where

import qualified Data.Text as T

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 $ preProcessInput $ T.pack content
    print $ solveQ2 $ preProcessInput $ T.pack content

preProcessInput :: T.Text -> [[T.Text]]
preProcessInput t = map (T.splitOn (T.pack "; ")) $ T.lines t >>= (tail . T.splitOn (T.pack ": ") . T.drop (length "Game "))

getCount :: T.Text -> T.Text -> Int
getCount suffix t = read . T.unpack $ T.dropEnd (T.length suffix) t

-- Q1
--
solveQ1 :: [[T.Text]] -> Int
solveQ1 xss = sum [i | (i,xs) <- zip [1..] xss, roundValid xs]

predicateBlue :: Int -> Bool
predicateBlue = (<= 14)

predicateGreen :: Int -> Bool
predicateGreen = (<= 13)

predicateRed :: Int -> Bool
predicateRed = (<= 12)

checkColor :: T.Text -> Bool
checkColor t
    | T.isSuffixOf blueSuffix t = predicateBlue $ getCount blueSuffix t
    | T.isSuffixOf greenSuffix t = predicateGreen $ getCount greenSuffix t
    | T.isSuffixOf redSuffix t = predicateRed $ getCount redSuffix t
    | otherwise = error "unknown suffix"
    where blueSuffix = T.pack " blue"
          greenSuffix = T.pack " green"
          redSuffix = T.pack " red"
    
roundValid :: [T.Text] -> Bool
roundValid xs = and . (map checkColor) . concat $ map (T.splitOn (T.pack ", ")) xs


-- Q2
--
solveQ2 :: [[T.Text]] -> Int
--solveQ2 xss = sum $ [(prod . minCountBlueGreenRed) xs | xs <- xss]
solveQ2 xss = sum [(prod . maxCountBlueGreenRed) (xs >>= T.splitOn (T.pack ", ")) 
                  | xs <- xss]

prod :: [Int] -> Int
prod = foldr (*) 1

maxCountBlueGreenRed :: [T.Text] -> [Int]
maxCountBlueGreenRed xs = [ maxCount blueSuffix xs
                          , maxCount greenSuffix xs
                          , maxCount redSuffix xs
                          ] where blueSuffix = T.pack " blue"
                                  greenSuffix = T.pack " green"
                                  redSuffix = T.pack " red"
                   
maxCount :: T.Text -> [T.Text] -> Int
maxCount suffix xs = maximum [getCount suffix x | x <- xs, T.isSuffixOf suffix x]
