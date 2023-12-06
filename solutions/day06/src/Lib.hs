module Lib
    ( printSol
    ) where

printSol :: String -> IO ()
printSol inputFilePath = do
    content <- readFile inputFilePath
    print $ solveQ1 content
    --print $ solveQ2 71530.0 940200.0
    print $ solveQ2 content

parseInput :: String -> [(Double, Double)]
parseInput content = zip (strippedLists !! 0) (strippedLists !! 1)
                     where strippedLists = map read . drop 1 . words <$> lines content

winningHoldingTimes :: (Double, Double) -> (Integer, Integer)
winningHoldingTimes (t,d) = (ceiling b1, floor b2)
                          where b1 = (-t + sqrt (t**2 - 4*d)) / (-2)
                                b2 = (-t - sqrt (t**2 - 4*d)) / (-2)

correctUp :: (Integer, Integer) -> Integer -> Integer
correctUp (t, d) b | b * (t - b) == d = b + 1
                   | otherwise        = b

correctDown :: (Integer, Integer) -> Integer -> Integer
correctDown (t, d) b | b * (t - b) == d = b - 1
                     | otherwise        = b

correct :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
correct param (b1, b2) = (correctUp param b1, correctDown param b2)



solveQ1 :: String -> Integer
solveQ1 content = foldr (\(l,u) acc -> (u-l+1) * acc) 1 bounds
                        where bounds = [correct (round t, round d) (b1, b2)
                                       | ((t, d), (b1, b2)) <- zip params candidates]
                              candidates = winningHoldingTimes <$> params
                              params = parseInput content

solveQ2 :: String -> Integer
solveQ2 content = u - l + 1
                  where (t, d) = parseInput2 content
                        (b1,b2) = winningHoldingTimes (t, d)
                        (l, u) = correct (round t, round d) (b1, b2)


parseInput2 :: String -> (Double, Double)
parseInput2 content = (xs !! 0, xs !! 1)
                    where xs = read . concat . drop 1 . words <$> lines content
