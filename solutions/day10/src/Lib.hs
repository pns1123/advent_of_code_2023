module Lib
    ( printSol
    ) where

import qualified Data.Map.Strict as M

printSol :: String -> IO ()
printSol filePath = do
    content <- readFile filePath
    let grid = getGrid $ lines content
    print $ solveQ1 grid [(30,118), (30,120)]
    print $ solveQ2 grid (30,118) (30,119) [(30,119)]

type Node = (Int, Int)
type Grid = [(Node, Char)]
type DistanceMap = M.Map Node Int

solveQ1 :: Grid -> [(Int, Int)] -> Int
solveQ1 grid startPoints = foldr max 0 $ foldr (M.unionWith min) M.empty distanceMaps
                         where s = findS grid
                               distanceMaps = [traverseGraph grid (M.fromList [(s,0)]) (v,0) s                                               | v <- startPoints]

getGrid :: [[Char]] -> Grid
getGrid css = [((row, col), c) |
               (row, cs) <- zip [0..] css, (col, c) <- zip [0..] cs]

findS :: Grid -> Node
findS grid = fst . head $ filter ((=='S') . snd) grid

traverseGraph :: Grid -> DistanceMap -> (Node, Int) -> Node -> DistanceMap
traverseGraph grid m (c,s) p | nextNode `M.member` m = M.insert c (s+1) m
                             | otherwise             = traverseGraph grid (M.insert c (s+1) m) (nextNode, s+1) c
                             where nextNode = next grid c p

next :: Grid -> Node -> Node -> Node
next grid cur prev = (row + incRow, col + incCol) --`debug` ("next = " ++ show((row + incRow, col + incCol)))
                   where (row, col) = cur
                         c = M.fromList grid M.! cur
                         (incRow, incCol) = transitionInc c cur prev

transitionInc :: Char -> Node -> Node -> Node
transitionInc c cur prev | c == '|' = (c_row-p_row, c_col-p_col)
                         | c == '-' = (c_row-p_row, c_col-p_col)
                         | c == 'L' = (c_col-p_col, c_row-p_row)
                         | c == 'J' = (p_col-c_col, p_row-c_row)
                         | c == 'F' = (p_col-c_col, p_row-c_row)
                         | c == '7' = (c_col-p_col, c_row-p_row)
                         | c == '.' = error ("landed on ground:" ++ show c ++ show cur ++ show prev)
                         | c == 'S' = error ("back on start:" ++ show c ++ show cur ++ show prev)
                         | otherwise = error "fell of grid"
                         where c_row = fst cur
                               c_col = snd cur
                               p_row = fst prev
                               p_col = snd prev

--Q2
--
solveQ2 :: Grid -> Node -> Node -> [Node] -> Int
solveQ2 grid c p vs = (picksInterior . transform) $ buildCircle grid c p vs

buildCircle :: Grid -> Node -> Node -> [Node] -> [Node]
buildCircle grid c p acc | nextNode `elem` acc = nextNode:c:acc
                         | otherwise             = buildCircle grid nextNode c (c:acc)
                         where nextNode = next grid c p

det :: Node -> Node -> Int
det (x1, y1) (x2, y2) = x1*y2 - x2*y1

shoelaceArea :: [Node] -> Int
shoelaceArea [v1,v2]    = det v1 v2
shoelaceArea (v1:v2:vs) = det v1 v2 + shoelaceArea (v2:vs)
shoelaceArea _          = error "there cannot one or no remaining nodes"

picksInterior :: [Node] -> Int
picksInterior vs = shoelaceArea vs `div` 2 - (length (tail vs) `div` 2) + 1

transform :: [Node] -> [Node]
transform vs = [(maxRow-x, y-minCol) | (x,y) <- vs]
             where maxRow = maximum $ fst <$> vs
                   minCol = minimum $ snd <$> vs
