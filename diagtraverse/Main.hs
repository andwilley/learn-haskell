module Main where

main :: IO ()
-- main = interact rev
main = print $ show $ addItemsFromRows [[]] ([[]], [[]])

addItemsFromRow :: [Int] -> [[Int]] -> [[Int]]
addItemsFromRow []    reslists = reslists
addItemsFromRow rowxs []       = addItemsFromRow rowxs [[]]
addItemsFromRow (x : rowxs) (y : reslists) =
  (x : y) : addItemsFromRow rowxs reslists

addItemsFromRows :: [[Int]] -> ([[Int]], [[Int]]) -> [Int]
addItemsFromRows []           (res, accum) = concat $ res ++ accum
addItemsFromRows (row : rows) (res, accum) = addItemsFromRows
  rows
  (res ++ [popRow], newRes)
  where (popRow : newRes) = addItemsFromRow row accum
