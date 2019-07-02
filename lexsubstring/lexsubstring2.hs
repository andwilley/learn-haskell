module Lexsubstring2 where

    import Data.List

    makeAlphaList :: String
    makeAlphaList = ['a'..'z']

    buildLexString :: Int -> [Int] -> String -> String -> Char
    buildLexString _ [] (a:alphaList) baseString = buildLexString (elemIndices a baseString) alphaList baseString
    buildLexString _ _ [] _ = error "index out of range"
    buildLexString stringIndex (i:is) alphaList baseString = 'a' -- getAndCheckEachLexSubs stringIndex (drop i baseString)

    -- getAndCheckEachLexSubs :: Int -> String -> (Char, String)
    -- getAndCheckEachLexSubs i baseString subStrSet
    --     | i <= length baseString = Set.insert (take i baseString) (getAndCheckEachLexSubs (i + 1) baseString subStrSet)
    --     | otherwise = subStrSet

    main :: IO ()
    main = print ""