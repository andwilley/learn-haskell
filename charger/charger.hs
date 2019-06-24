module Charger where
    main :: IO ()
    main = do
        content <- getContents
        putStrLn (foldl (\x y -> x ++ y ++ "\r\n") "" (eachLine content))

    eachLine :: String -> [String]
    eachLine content = mapWIndex reducePower allLines
        where
            _:rest = lines content
            allLines = map (lineTuple . words) rest

    lineTuple :: [String] -> (Int, String)
    lineTuple [power, code] = (read power :: Int, code)
    lineTuple x = error ("Can't do that " ++ show x)

    mapWIndex :: (a -> Int -> b) -> [a] -> [b]
    mapWIndex f list = zipWith f list [1..]

    reducePower' :: Int -> (Int, String) -> Int -> String
    reducePower' iter (capacity, code) caseNum
        | capacity >= damage = "Case #" ++ show caseNum ++ ": " ++ show iter
        | reducedCode == code = "Case #" ++ show caseNum ++ ": IMPOSSIBLE"
        | otherwise = reducePower' (iter + 1) (capacity, reducedCode) caseNum
        where damage = calcDamage code
              reducedCode = reduceByMaxOnce code

    -- use 0 as the initial iteration
    reducePower :: (Int, String) -> Int -> String
    reducePower = reducePower' 0

    calcDamage' :: Int -> String -> Int
    calcDamage' _ [] = 0
    calcDamage' charge (x:xs) = case x of
        'S' -> charge + calcDamage' charge xs
        'C' -> calcDamage' (charge * 2) xs
        _ -> calcDamage' charge xs

    -- use 1 as the initial charge
    calcDamage :: String -> Int
    calcDamage = calcDamage' 1

    reduceByMaxOnce :: String -> String
    reduceByMaxOnce = reverse . swapHighest . reverse

    swapHighest :: String -> String
    swapHighest [] = []
    swapHighest [x] = [x]
    swapHighest (x:y:rest)
        | x == 'S' && y == 'C' = y:x:rest
        | otherwise = x : swapHighest (y:rest)
