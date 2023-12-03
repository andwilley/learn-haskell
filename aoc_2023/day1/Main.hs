module Main where

import Data.Char
import Data.Map

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ doWork $ lines content

doWork :: [String] -> Int
doWork ls = sum $ fmap (getCodes '0' '0' False) ls

getCodes :: Char -> Char -> Bool -> String -> Int
getCodes first last seen [] = read [first, last]
getCodes first last seen (x:xs) =
  if isDigit x then getCodes f x True xs else getCodes first last seen xs
  where f = if seen then first else x

-- numTrie :: Map String (Maybe (Map Stri uug )

newtype Trie = Trie (Map Char (Maybe Trie))

newtype Tb = Tb ([(Char, Tb)])

numTrie :: Trie
numTrie = mt $ Tb [
              ('o', Tb [('n', Tb [('e', Tb [])])]),
              ('t',
                Tb [
                  ('w', Tb [('o', Tb [])]),
                  ('h', Tb [('r', Tb [('e', Tb [('e', Tb [])])])])
                ]),
              ('f', Tb [
                  ('o', Tb [('u', Tb [('r', Tb [])])]),
                  ('i', Tb [('v', Tb [('e', Tb [])])])
              ]),
              ('s', Tb [
                ('i', Tb [('x', Tb [])]),
                ('e', Tb [('v', Tb [('e', Tb [('n', Tb [])])])])
              ]),
              ('e', Tb [('i', Tb [('g', Tb [('h', Tb [('t', Tb [])])])])]),
              ('n', Tb [('i', Tb [('n', Tb [('e', Tb [])])])])
            ]

mt :: Tb -> Trie
-- Last item in the nested list, return a map of that char to Nothing
mt (Tb items) = Prelude.foldl buildMap (Trie empty) items

buildMap :: Trie -> (Char, Tb) -> Trie
buildMap (Trie m) (char, (Tb [])) = Trie $ insert char Nothing m
buildMap (Trie m) (char, Tb ls) = Trie $ insert char (Just (Prelude.foldl buildMap (Trie empty) ls)) m
