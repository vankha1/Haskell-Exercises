
-- Square sum of list using foldr
sumSquare :: [Int] -> Int
sumSquare xs = foldr (\x temp  -> x  *x + temp) 0 xs