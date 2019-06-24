
    main :: IO ()
    main = do
        content <- getContents
        putStrLn (foldl (\x y -> x ++ y ++ "\r\n") "" (eachLine content))

    eachLine :: String -> [String]
    eachLine content = mapWIndex cutWaffles testCases
        where
            _:rest = lines content
            testCases = getTestCases rest

    mapWIndex :: (a -> Int -> b) -> [a] -> [b]
    mapWIndex f list = zipWith f list [1..]

    getTestCases' :: [[String]] -> [String] -> [String] -> [[String]]
    getTestCases' _ testCase [] = [testCase]
    getTestCases' testCases testCase ([]:restOfArray) = getTestCases' testCases testCase restOfArray
    getTestCases' testCases testCase (nextLine@(firstChar:_):restOfArray)
        | isInteger [firstChar] = testCases ++ getTestCases' testCases [nextLine] restOfArray
        | otherwise = getTestCases' testCases (testCase ++ [nextLine]) restOfArray

    getTestCases :: [String] -> [[String]]
    getTestCases = getTestCases' [] []

    cutWaffles :: [String] -> Int -> String
    cutWaffles testCase caseNum = "Case #" ++ show caseNum ++ ": " ++ show testCase

    isInteger :: String -> Bool
    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False