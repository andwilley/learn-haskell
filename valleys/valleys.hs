module Valleys where
    -- every time the counter goes from 0 to -1, its a valley
    countDrops :: (Int, Int) -> Char -> (Int, Int)
    countDrops (level, numDrops) char
        | char == 'D' = (level - 1, newDrops)
        | char == 'U' = (level + 1, numDrops)
        | otherwise = error "bad input"
            where newDrops = if level == 0 then numDrops + 1 else numDrops

    solve :: String -> Int
    solve xs = snd $ foldl countDrops (0, 0) xs

    main :: IO ()
    main = do
        _ <- getLine
        content <- getLine
        print $ solve content
