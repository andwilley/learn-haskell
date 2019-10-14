module Leaderboard where
    import qualified Data.Map as Map
    import Data.Maybe

    -- map => score: (index, rank)
    -- lastScore rank index historyTable board score -> (rank, map)
    findRank' :: Int -> Int -> Int -> Map.Map Int (Int, Int) -> [Int] -> Int -> (Int -> Int) -> (Int, Map.Map Int (Int, Int), Int)
    findRank' _ rank index historyTable [] _ _ = (rank, historyTable, index)
    findRank' lastScore rank index historyTable (s:scoreBoard) score hash
        -- first time, score is in the table, use rank and index from table,
        | lastScore == (-1) && isJust tableVal = findRank' (-2) (snd $ fromJust tableVal)
            (fst $ fromJust tableVal) historyTable (drop (fst $ fromJust tableVal) scoreBoard) score hash
        | s == lastScore = findRank' s rank (index + 1) historyTable scoreBoard score hash
        | score < s = findRank' s (rank + 1) (index + 1) historyTable scoreBoard score hash
        | otherwise = (rank, historyTable, index)
        where tableVal = Map.lookup (hash score) historyTable

    findRank :: Map.Map Int (Int, Int) -> [Int] -> Int -> (Int -> Int) -> (Int, Map.Map Int (Int, Int), Int)
    findRank = findRank' (-1) 1 0

    solve :: Map.Map Int (Int, Int) -> [Int] -> [Int] -> [String]
    solve _ _ [] = []
    solve table board (s:scores) = show (first rankAndMap) : solve (second rankAndMap) board scores
        where rankAndMap = findRank table board s (hash' $ length board)

    makeHistoryTable :: [Int] -> [Int] -> Map.Map Int (Int, Int)
    makeHistoryTable _ [] = Map.fromList []
    makeHistoryTable board (i:intervals)
        | length board < 10 = Map.fromList []
        | otherwise = Map.insert i (index, rank) (makeHistoryTable board intervals)
        where (rank,_,index) = findRank (Map.fromList []) board i (hash' $ length board)

    makeIntervalList :: Int -> [Int]
    makeIntervalList boardLength
        | boardLength < 10 = []
        | otherwise = [interval,interval * 2..interval * 10]
        where interval = boardLength `div` 10

    -- score hashes to a slightly higher score, but similarly grouped scores do too so they find eachother
    hash' :: Int -> Int -> Int
    hash' boardLength x
        | boardLength < 10 = x
        | otherwise = (hashDiv - x `mod` hashDiv) + x
        where hashDiv = boardLength `div` 10

    first :: (a, b, c) -> a
    first (x,_,_) = x
    second :: (a, b, c) -> b
    second (_,y,_) = y
    third :: (a, b, c) -> c
    third (_,_,z) = z

    main :: IO ()
    main = do
        _ <- getLine
        scoreBoard <- getLine
        _ <- getLine
        scores <- getLine
        putStrLn $ unlines $ reverse $ solve (makeHistoryTable (map read (words scoreBoard)) (makeIntervalList (length scoreBoard))) (map read (words scoreBoard)) (map read (reverse (words scores)))
