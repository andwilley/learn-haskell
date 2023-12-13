module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  putStr contents
