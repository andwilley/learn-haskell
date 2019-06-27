module Sumpairs where
    solve :: [Int] -> [Int] -> Int -> Int
    solve [] _ _ = 0
    solve [_] _ _ = 0
    solve _ [] _ = error "nope"
    solve (_:xs) [_] k = solve xs xs k
    solve allx@(x:_) (_:ys) k
        | (x + head ys) `mod` k == 0 = 1 + solve allx ys k
        | otherwise = solve allx ys k

    main :: IO ()
    main = do
        nk <- getLine
        wholeStr <- getLine
        print $ solve (map read (words wholeStr)) (map read (words wholeStr)) (read $ head $ tail (words nk))