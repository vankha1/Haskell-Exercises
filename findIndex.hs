
-- Find index of a element 
findElement :: Num a => Eq a => a -> [a] -> a

findElement _ [] = -1
findElement a xs = go 0 xs
  where 
    go i [] = -1
    go i (y:ys)
      | a == y = i
      | otherwise = go (i + 1) ys
