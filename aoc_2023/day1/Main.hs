module Main where

import Data.Maybe
import Data.Set as Set
import Data.Char
import Data.Map as Map

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ doWork $ lines content

doWork :: [String] -> Int
doWork ls = sum $ fmap (getCodes '0' '0' False Set.empty) ls

getCodes :: Char -> Char -> Bool -> Set String -> String -> Int
getCodes first last seen bufs [] = read [first, last]
getCodes first last seen bufs (x:xs) =
  if isDigit x
  then getCodes f x True Set.empty xs
  else getCodes first last seen Set.empty xs
  where f = if seen then first else x

setBuffers :: Char -> Set String -> Set String
setBuffers char bufs = Set.empty

data TriePresence = Zero | Partial | Full

checkTrie :: String -> Trie -> Bool
checkTrie [] (Trie _) = Partial
checkTrie (first:rest) (Trie ) = Zero
checkTrie (first:rest) (Trie trie) | isJust nestedTrie = fromMaybe Full (fmap $ checkTrie rest nestedTrie)
                                   | otherwise         = Zero
  where nestedTrie = fromMaybe Nothing (Map.lookup first trie)



newtype Trie = Trie (Map Char (Maybe Trie))
newtype TrieBuilder = Tb ([(Char, Tb)])
type Tb = TrieBuilder

mt :: Tb -> Trie
-- Last item in the nested list, return a map of that char to Nothing
mt (Tb items) = Prelude.foldl buildMap (Trie Map.empty) items

buildMap :: Trie -> (Char, Tb) -> Trie
buildMap (Trie m) (char, (Tb [])) = Trie $ Map.insert char Nothing m
buildMap (Trie m) (char, ls) = Trie $ Map.insert char (Just $ mt ls) m

-- Precomputed Trie
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


-- does it match a first key
--   if so, add it to the set
-- does is continue to match if added to any in progress buffers
--   if so, add it to those
-- for any it doesn't match, delete those keys
