
-- Check all elements of array being square

isSquare :: Int -> Bool
isSquare n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n :: Double)  -- sq = floor (sqrt  (fromIntegral n :: Double))

checkSquare :: [Int] -> Bool

checkSquare [] = True
checkSquare [x] = isSquare x
checkSquare (x:xs) = isSquare x && checkSquare xs
