module Lib
    ( printSol
    ) where

import           Data.List       (nub)
import qualified Data.Map.Strict as M

printSol :: String -> IO ()
printSol filePath = do
    content <- readFile filePath
    let grid = getGrid $ lines content
    print $ solveQ1 grid [(30,118), (30,120)]

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

neigh :: Grid -> Node -> [Node]
neigh grid (row, col) = [v | v <- [(row-1,col), (row,col+1),
                                   (row+1,col), (row, col-1)],
                             v `M.member` M.fromList grid]

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
buildCircle :: Grid -> Node -> Node -> [Node] -> [Node]
buildCircle grid c p acc | nextNode `elem` acc = c:acc
                         | otherwise             = buildCircle grid nextNode c (c:acc)
                         where nextNode = next grid c p


getBoundary :: Grid -> [Node]
getBoundary grid = [(row, col) | ((row, col),_) <- grid,
                                 row == 0 || col == 0 || row == maxRow || col == maxCol  ]
                   where maxRow = maximum [row | ((row,_),_) <- grid]
                         maxCol = maximum [col | ((_,col),_) <- grid]

expand :: Grid -> [Node] -> Node -> [Node]
expand grid circle v = [n | n <- neigh grid v, n `notElem` circle]

expandOuter :: Grid -> [Node] -> [Node] -> [Node]
expandOuter grid circle outer = case filter (not . (`elem` outer)) (concat [expand grid circle v | v <- outer]) of
                                [] -> outer
                                vs -> expandOuter grid circle (vs ++ outer)
