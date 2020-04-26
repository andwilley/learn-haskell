module Prisoners where
    solve :: String -> Int
    solve inputs = ((offset + s - 1) `mod` n) + 1
        where (n, m, s) = getInputs $ map read $ words inputs;
              offset    = m `mod` n;

    getInputs :: [Int] -> (Int, Int, Int)
    getInputs [n, m, s] = (n, m, s-1)
    getInputs _ = error "bad input"

    main :: IO ()
    main = interact $ unlines . map (show . solve) . tail . lines
