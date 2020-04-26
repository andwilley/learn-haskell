module Main where

import qualified Data.Map as Map
import Control.Monad.State

type CountMap = Map.Map Char Int

main :: IO ()
main = interact $ isAnagram . getWordPair

getWordPair :: String -> [String]
getWordPair = take 2 . lines

isAnagram :: [String] -> String
isAnagram (x:y:_) = if execState (countChars x) initialState == execState (countChars y) initialState
  then "true"
  else "false"
isAnagram _ = "false"

countChars :: String -> State CountMap ()
countChars [] = return ()
countChars (x:xs) = do
  countChar x
  countChars xs

countChar :: Char -> State CountMap ()
countChar x = do
  countMap <- get
  case Map.lookup x countMap of
    Just val -> put $ Map.insert x (val + 1) countMap
    Nothing -> put $ Map.insert x 1 countMap

initialState :: CountMap
initialState = Map.fromList []
