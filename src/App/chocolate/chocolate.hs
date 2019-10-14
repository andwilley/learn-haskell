module Chocolate where
    solve :: [Int] -> [Int] -> Int
    solve [] _ = 0
    solve _ [] = error "nope"
    solve _ [_] = error "nope"
    solve chocolate (d:m:_)
        | sum (take m chocolate) == d = 1 + solve (drop 1 chocolate) [d,m]
        | otherwise = solve (drop 1 chocolate) [d,m]

    main :: IO ()
    main = do
        _ <- getLine
        chocolate <- getLine
        dates <- getLine
        print (solve (map read (words chocolate)) (map read (words dates)))