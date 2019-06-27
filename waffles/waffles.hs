module Waffles where
    import qualified Data.Map as Map

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

    getTestCases :: [String] -> [[String]]
    getTestCases [] = []
    getTestCases ([]:rest) = getTestCases rest
    getTestCases xs@((firstChar:_):_)
        | isInteger [firstChar] = take ((read [firstChar] :: Int) + 1) xs : getTestCases (drop ((read [firstChar] :: Int) + 1) xs)
        | otherwise = []


    isInteger :: String -> Bool
    isInteger s = case reads s :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False

    cutWaffles :: [String] -> Int -> String
    cutWaffles (spec:waffle) caseNum = "Case #" ++ show caseNum ++ ": " ++ show (checkSquares hCuts vCuts waffle chipsPerSquare)
        where rows = Map.fromList $ zip [0..(read r :: Int)-1] [0,0..]
              cols = Map.fromList $ zip [0..(read c :: Int)-1] [0,0..]
              (r:c:h:v:_) = words spec
              (rowCnt, colCnt) = countChips (rows, cols) waffle
              hCuts = take 3 $ makeCuts 1 rowCnt (read h :: Int) (foldl (\a b -> a + snd b) 0 rowCnt) 0
              vCuts = take 3 $ makeCuts 1 colCnt (read v :: Int) (foldl (\a b -> a + snd b) 0 colCnt) 0
              chipsPerSquare = foldl (\a b -> a + snd b) 0 rowCnt `div` ((read h :: Int) + 1) `div` ((read v :: Int) + 1)
    cutWaffles _ _ = ""

    countChips' :: Int -> (Map.Map Int Int, Map.Map Int Int) -> [String] -> ([(Int, Int)], [(Int, Int)])
    countChips' _ (rows, cols) [] = (Map.toList rows, Map.toList cols)
    countChips' rowIndex (rows, cols) (nextLine:restOfWaffle) = countChips' (rowIndex + 1) (updRows, updCols) restOfWaffle
        where ((updRows, updCols), _) = foldl (countChipsInRow rowIndex) ((rows, cols), 0) nextLine

    countChips :: (Map.Map Int Int, Map.Map Int Int) -> [String] -> ([(Int, Int)], [(Int, Int)])
    countChips = countChips' 0

    countChipsInRow :: Int -> ((Map.Map Int Int, Map.Map Int Int), Int) -> Char -> ((Map.Map Int Int, Map.Map Int Int), Int)
    countChipsInRow rowIndex ((rows, cols), colIndex) content
        | content == '@' = ((updRows, updCols), colIndex + 1)
        | otherwise = ((rows, cols), colIndex + 1)
        where updRows = Map.insert rowIndex (maybe 1 (+1) (Map.lookup rowIndex rows)) rows
              updCols = Map.insert colIndex (maybe 1 (+1) (Map.lookup colIndex cols)) cols

    -- return the cut rows/cols as the number of rows from the previous cut
    makeCuts :: Int -> [(Int, Int)] -> Int -> Int -> Int -> [Int]
    makeCuts _ [] _ _ _ = []
    makeCuts _ [_] _ _ _ = []
    makeCuts lastCut ((_, cnt):lanes) numCuts total runningTotal
        | total == 0 = [1]
        | total `mod` (numCuts + 1) > 0 = []
        | runningTotal + cnt == total `div` (numCuts + 1) = lastCut : makeCuts 1 lanes numCuts total 0
        | runningTotal + cnt < total `div` (numCuts + 1) = makeCuts (lastCut + 1) lanes numCuts total (runningTotal + cnt)
        | otherwise = []

    checkSquares :: [Int] -> [Int] -> [String] -> Int -> String
    checkSquares _ [] _ _ = "IMPOSSIBLE"
    checkSquares [] vCuts slice chipsPerSquare
        | possible = "POSSIBLE"
        | otherwise = "IMPOSSIBLE"
        where possible = checkRowSlices slice vCuts chipsPerSquare
    checkSquares _ _ [] _ = error "uh oh"
    checkSquares (a:hCuts) vCuts (b:waffle) chipsPerSquare
        | possible = checkSquares hCuts vCuts (drop a (b:waffle)) chipsPerSquare
        | otherwise = "IMPOSSIBLE"
        where possible = checkRowSlices rows vCuts chipsPerSquare
              rows = if a <= 1 then [b]
                     else take a (b:waffle)

    checkRowSlices :: [String] -> [Int] -> Int -> Bool
    checkRowSlices _ [] _ = True
    checkRowSlices slices (a:vCuts) chipsPerSquare
        | possible = checkRowSlices (map (drop a) slices) vCuts chipsPerSquare
        | otherwise = False
        where possible = length (filter (== '@') (concatMap (take a) slices)) == chipsPerSquare
