module Birds where
    import Data.List
    import Data.Function

    count :: [Int] -> Int
    count = head . minimumBy (flip compare `on` length) . group . sort

    main :: IO ()
    main = interact $ show . count . map read . tail . words