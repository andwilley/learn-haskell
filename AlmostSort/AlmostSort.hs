module AlmostSort where
    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy p = map (uncurry (:)) . groupByNonEmpty p

    groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [(a,[a])]
    groupByNonEmpty p =
        foldr
        (\x0 yt ->
            let (xr,yr) =
                    case yt of
                    (x1,xs):ys ->
                        if p x0 x1
                            then (x1:xs,ys)
                            else ([],yt)
                    [] -> ([],yt)
            in  (x0,xr):yr)
        []

    sepByDir :: [Int] -> [[Int]]
    sepByDir []         = []
    sepByDir [x]        = [[x]]
    sepByDir [l1:l2:ls] = sepByDir ls ++ [[l1, l2]]
