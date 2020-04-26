module Lexsubstring where
    import qualified Data.Set as Set
    import Data.List

    getAllLexSubs :: String -> Set.Set String
    getAllLexSubs [] = Set.fromList []
    getAllLexSubs (b:baseString) = getEachLexSubs (b:baseString) (getAllLexSubs baseString)

    getEachLexSubs' :: Int -> String -> Set.Set String -> Set.Set String
    getEachLexSubs' i baseString subStrSet
        | i <= length baseString = Set.insert (take i baseString) (getEachLexSubs' (i + 1) baseString subStrSet)
        | otherwise = subStrSet

    getEachLexSubs :: String -> Set.Set String -> Set.Set String
    getEachLexSubs = getEachLexSubs' 1

    solve :: [String] -> [String]
    solve [] = []
    solve [_] = error "nope"
    solve (a:b:content) = [intercalate "" (Set.toAscList (getAllLexSubs a))!!(read b - 1)] : solve content

    main :: IO ()
    main = do
        _ <- getLine
        content <- getContents
        putStrLn $ unlines $ solve (lines content)
