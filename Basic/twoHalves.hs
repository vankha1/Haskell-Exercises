-- Split even-length list into two halves
halve :: [a] -> ([a], [a])

halve xs = 
  let len = length xs
  in (take (len `div` 2) xs, drop (len `div` 2) xs)
