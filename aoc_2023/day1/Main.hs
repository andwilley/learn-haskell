module Main where

import Data.Char

main :: IO ()
main = do
  content <- readFile "test_input.txt"
  print $ doWork $ lines content


doWork :: [String] -> Int
doWork ls = sum $ map (getCodes '0' '0' False) ls

getCodes :: Char -> Char -> Bool -> String -> Int
getCodes first last seen [] = read [first, last]
getCodes first last seen (x:xs) = if isDigit x then getCodes f x True xs else getCodes first last seen xs
  where f = if seen then first else x
