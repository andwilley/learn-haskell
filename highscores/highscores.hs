module Highscores where
    test :: [Int]
    test = [12,24,10,12]

    solve :: String -> String
    solve input = getRecords (-1) (-1) scores 0 0
        where scores = map read $ words $ head $ tail $ lines input :: [Int]

    getRecords :: Int -> Int -> [Int] -> Int -> Int -> String
    getRecords _ _ [] hcnt lcnt = show hcnt ++ " " ++ show lcnt
    getRecords high low (s:scores) hcnt lcnt
        | high == (-1) = getRecords s s scores hcnt lcnt
        | s > high = getRecords s low scores (hcnt + 1) lcnt
        | s < low = getRecords high s scores hcnt (lcnt + 1)
        | otherwise = getRecords high low scores hcnt lcnt

    main :: IO ()
    main = interact solve