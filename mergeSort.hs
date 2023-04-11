
merge :: Ord a => [a]->[a]->[a]

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]

mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs =
  let leftArr = mergeSort (take ((length xs) `div` 2) xs)
      rightArr = mergeSort (drop ((length xs) `div` 2) xs)
  in merge leftArr rightArr