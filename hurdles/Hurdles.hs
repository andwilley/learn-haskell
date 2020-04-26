module Hurdles where

    solve :: Int -> [Int] -> Int
    solve h hs = max (maximum hs - h) 0

    main :: IO ()
    main = do
        first <- getLine
        let [_, h] = words first
        second <- getLine
        print $ solve (read h) (map read $ words second)
