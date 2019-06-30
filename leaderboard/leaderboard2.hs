-- module Leaderboard2 where
    import Data.Array;

    binarySearch :: (Ord a, Num a, Integral a) =>
        Array Int a -> a -> Int -> Int -> Int
    binarySearch haystack needle l r
        | r < l = l
        | pivot < needle = binarySearch haystack needle l (mid-1)
        | pivot > needle = binarySearch haystack needle (mid+1) r
        | otherwise = mid
        where
            mid = l + (r-l) `div` 2
            pivot = haystack!mid

    arrayFromList :: [a] -> Array Int a
    arrayFromList anyList = array (0, length anyList - 1) (zip [0..] anyList)

    buildRankList :: [Int] -> Int -> Int -> [Int]
    buildRankList [] _ _ = []
    buildRankList (b:board) lastScore rank
        | b < lastScore = (rank + 1) : buildRankList board b (rank + 1)
        | otherwise = rank : buildRankList board b rank

    solveOne :: Array Int Int -> Array Int Int -> Int -> Int
    solveOne board ranks score = rank
        where i = binarySearch board score 0 (length board - 1)
              rank = if i < length board then ranks!i else (ranks!(length board - 1)) + 1

    solveAll :: (Int -> Int) -> [Int] -> [String]
    solveAll _ [] = []
    solveAll solveBoard (s:scores) = show rank : solveAll solveBoard scores
        where rank = solveBoard s

    main :: IO ()
    main = do
        _ <- getLine
        scoreBoard <- getLine
        _ <- getLine
        scoresString <- getLine
        let board = map read (words scoreBoard)
        let scores = map read (words scoresString)
        let solveBoard = solveOne (arrayFromList board) (arrayFromList (buildRankList board (-1) 1))
        -- print $ solveBoard (head scores)
        putStrLn $ unlines $ solveAll solveBoard scores