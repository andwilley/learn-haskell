module Main (main) where

import Lib

main :: IO ()
main = do
  contents <- getContents
  putStr contents
