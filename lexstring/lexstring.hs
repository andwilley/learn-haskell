zipLex :: String -> String -> (Bool, Int) -> String
zipLex [] [] _ = ""
zipLex first [] _ = first
zipLex [] second _ = second
zipLex (f:first) (s:second) (sndIsLowest, reps)
    | f == s && fst scndIsLow = s : zipLex (f:first) second scndIsLow
    | f == s = f : zipLex first (s:second) (True, 0)
    | f < s = f : zipLex first (s:second) (True, 0)
    | otherwise = s : zipLex (f:first) second (True, 0)
    where scndIsLow = if reps > 0 then (sndIsLowest, reps) else secondIsLowest first second 0

-- dont repeat this if we already seen they're equal to a certain point
secondIsLowest :: String -> String -> Int -> (Bool, Int)
secondIsLowest [] [] i = (False, i)
secondIsLowest _ [] i = (False, i)
secondIsLowest [] _ i = (True, i)
secondIsLowest (f:first) (s:second) i
    | f > s = (True, i)
    | f < s = (False, i)
    | otherwise = secondIsLowest first second (i + 1)

solve :: [String] -> [String]
solve [] = []
solve [_] = error "nope"
solve (a:b:content) = zipLex a b (True, 0) : solve content

main :: IO ()
main = do
    _ <- getLine
    content <- getContents
    putStrLn $ unlines $ solve $ lines content
