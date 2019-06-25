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
cutWaffles (spec:waffle) caseNum = "Case #" ++ show caseNum ++ ": " ++ show (checkSquares hCuts vCuts cnt)
    where rows = Map.fromList $ zip [0..(read r :: Int)-1] [0,0..]
          cols = Map.fromList $ zip [0..(read c :: Int)-1] [0,0..]
          (r:c:h:v:_) = words spec
          cnt@(rowCnt, colCnt) = countChips (rows, cols) waffle
          hCuts = makeCuts rowCnt (read h :: Int) (foldl (\a b -> a + snd b) 0 rowCnt) 0
          vCuts = makeCuts colCnt (read v :: Int) (foldl (\a b -> a + snd b) 0 colCnt) 0
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

makeCuts :: [(Int, Int)] -> Int -> Int -> Int -> [Int]
makeCuts [] _ _ _ = []
makeCuts [_] _ _ _ = []
makeCuts ((lane, cnt):lanes) numCuts total runningTotal
    | total == 0 = [1]
    | total `mod` (numCuts + 1) > 0 = []
    | runningTotal + cnt == total `div` (numCuts + 1) = lane + 1 : makeCuts lanes numCuts total 0
    | runningTotal + cnt < total `div` (numCuts + 1) = makeCuts lanes numCuts total (runningTotal + cnt)
    | otherwise = []

checkSquares :: [Int] -> [Int] -> ([(Int, Int)], [(Int, Int)]) -> String
checkSquares [] _ _ = "IMPOSSIBLE"
checkSquares _ [] _ = "IMPOSSIBLE"
checkSquares hCuts vCuts cnt = ""