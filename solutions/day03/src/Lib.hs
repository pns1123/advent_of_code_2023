{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib
    ( printSol
    ) where

import           Data.Char
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.List.Split     (wordsBy)
import           GHC.Generics

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    print $ solveQ2 content


preProcess :: String -> [[(Char, Coordinate)]]
preProcess input = [[(c, Coordinate{row = i, col=j})
                   | (j, c) <- zip [1..] l]
                   | (i, l) <- zip [1..] $ lines input]

data Coordinate = Coordinate {row :: Int, col :: Int} deriving (Show, Eq, Generic, Hashable)

data BoundingBox = BoundingBox { num         :: Int
                               , topLeft     :: Coordinate
                               , bottomRight :: Coordinate
                               } deriving (Show, Eq)


solveQ1 :: String -> Int
solveQ1 content = sum $ map num $ filter (containsPuzzleSymbol (lines content)) $ assembleBoxes $ preProcess content

solveQ2 :: String -> Int
solveQ2 content = sum $ map product $  HM.elems $ HM.filter (\l -> length l == 2) $ gearMap $ filter adjacentToStar $ extractCharCoordinates content

extractCharCoordinates :: String -> [[(Char, Coordinate)]]
extractCharCoordinates content = map (concat . retrieveBox2 (preProcess content)) (assembleBoxes $ preProcess content)

adjacentToStar :: [(Char, Coordinate)] -> Bool
adjacentToStar xs = or ((== '*') . fst <$> xs)

gearMap :: [[(Char, Coordinate)]] -> HM.HashMap Coordinate [Int]
gearMap = foldr (HM.unionWith (++) . extractStars) HM.empty

extractStars :: [(Char, Coordinate)] -> HM.HashMap Coordinate [Int]
extractStars xs = let starsCoordinates = snd <$> filter ((=='*') . fst) xs
                      _num = extractNum $ filter (isDigit . fst) xs
                  in HM.fromList [(sC, [_num]) | sC <- starsCoordinates]

assembleBoxes :: [[(Char, Coordinate)]] -> [BoundingBox]
assembleBoxes input = [BoundingBox{num = extractNum x,
                             topLeft = (fst . corners) x,
                             bottomRight = (snd . corners) x} |
                 l <- input, x <- locateNums l]
                 where maxRow = length input
                       maxCol = (length . head) input
                       corners x = computeCorners maxRow maxCol (snd $ head x) (snd $ last x)


computeCorners :: Int -> Int -> Coordinate -> Coordinate -> (Coordinate, Coordinate)
computeCorners maxX maxY start end = (_topLeft, _bottomRight)
                                     where _topLeft = Coordinate { row = max 0 (row start - 1)
                                                                 , col = max 0 (col start - 1)
                                                                 }
                                           _bottomRight = Coordinate { row = min maxX (row end + 1)                                                                   , col = min maxY (col end + 1)
                                                                     }
locateNums :: [(Char, Coordinate)] -> [[(Char, Coordinate)]]
locateNums = wordsBy (isWordSep . fst)

extractNum :: [(Char, Coordinate)] -> Int
extractNum = read . map fst

retrieveBox :: BoundingBox -> [String] -> [String]
retrieveBox bound grid = [ (drop (leftCol - 1) . take rightCol) l
                         | l <- (drop (topRow - 1) . take bottomRow) grid]
                         where topRow = (row . topLeft) bound
                               bottomRow = (row . bottomRight) bound
                               leftCol = (col . topLeft) bound
                               rightCol = (col . bottomRight) bound

retrieveBox2 :: [[a]] -> BoundingBox -> [[a]]
retrieveBox2 grid bound = [ (drop (leftCol - 1) . take rightCol) l
                          | l <- (drop (topRow - 1) . take bottomRow) grid]
                          where topRow = (row . topLeft) bound
                                bottomRow = (row . bottomRight) bound
                                leftCol = (col . topLeft) bound
                                rightCol = (col . bottomRight) bound

containsPuzzleSymbol :: [String] -> BoundingBox -> Bool
containsPuzzleSymbol ls box =  any isPuzzleSymbol $ concat $ retrieveBox box ls

isPuzzleSymbol :: Char -> Bool
isPuzzleSymbol c = (not . isDigit) c && (not . isLetter) c && (c /= '.')

isWordSep :: Char -> Bool
isWordSep c = isPuzzleSymbol c || c == '.'
