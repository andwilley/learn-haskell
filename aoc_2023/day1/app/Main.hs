module Main where

import ClassyPrelude

main :: IO ()
main = print $ checkTrie "one" (Just numTrie)

-- main :: IO ()
-- main = do
--   content <- readFile "input.txt"
--   print $ doWork $ lines content
doWork :: [String] -> Int
doWork ls = sum $ fmap (getCodes '0' '0' False mempty) ls

getCodes :: Char -> Char -> Bool -> Set String -> String -> Int
getCodes first last seen bufs [] = read [first, last]
getCodes first last seen bufs (x:xs) =
  if isDigit x
    then getCodes f x True mempty xs
    else getCodes first last seen mempty xs
  where
    f =
      if seen
        then first
        else x

setBuffers :: Char -> Set String -> Set String
setBuffers char bufs = mempty

data TriePresence
  = Zero
  | Partial
  | Full
  deriving (Show)

checkTrie :: String -> Maybe Trie -> TriePresence
checkTrie [] Nothing = Full
checkTrie (l:ls) Nothing = Zero
checkTrie [] (Just (Trie trie)) = Partial
checkTrie (c:cs) (Just trie) =
  if isRight nested
    then checkTrie cs (fromRight Nothing nested)
    else Zero
  where
    nested = unnestTrieLookup c (Just trie)

unnestTrieLookup :: Char -> Maybe Trie -> Either () (Maybe Trie)
unnestTrieLookup c Nothing = Left ()
unnestTrieLookup c (Just (Trie trie)) =
  if isTrie
    then Right $ fromJust value
    else Left ()
  where
    value = lookup c trie
    present = isJust value
    isTrie = present && isJust (fromJust value)

newtype Trie =
  Trie (Map Char (Maybe Trie))

newtype TrieBuilder =
  Tb ([(Char, Tb)])

type Tb = TrieBuilder

mt :: Tb -> Trie
-- Last item in the nested list, return a map of that char to Nothing
mt (Tb items) = foldl buildMap (Trie mempty) items

buildMap :: Trie -> (Char, Tb) -> Trie
buildMap (Trie m) (char, (Tb [])) = Trie $ asMap $ insert char Nothing m
buildMap (Trie m) (char, ls) = Trie $ asMap $ insert char (Just $ mt ls) m

-- Precomputed Trie
numTrie :: Trie
numTrie =
  mt $
  Tb
    [ ('o', Tb [('n', Tb [('e', Tb [])])])
    , ( 't'
      , Tb
          [ ('w', Tb [('o', Tb [])])
          , ('h', Tb [('r', Tb [('e', Tb [('e', Tb [])])])])
          ])
    , ( 'f'
      , Tb
          [ ('o', Tb [('u', Tb [('r', Tb [])])])
          , ('i', Tb [('v', Tb [('e', Tb [])])])
          ])
    , ( 's'
      , Tb
          [ ('i', Tb [('x', Tb [])])
          , ('e', Tb [('v', Tb [('e', Tb [('n', Tb [])])])])
          ])
    , ('e', Tb [('i', Tb [('g', Tb [('h', Tb [('t', Tb [])])])])])
    , ('n', Tb [('i', Tb [('n', Tb [('e', Tb [])])])])
    ]
