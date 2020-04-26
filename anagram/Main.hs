module Main where

main :: IO ()
main = interact rev

rev :: String -> String
rev = reverse

