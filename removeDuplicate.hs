-- Remove duplicate element
removeDuplicate :: Eq a => [a] -> [a]

removeDuplicate [] = []
removeDuplicate (x:xs) = x : removeDuplicate (filter (/= x) xs)

-- Remove duplicate and consecutive character
compress :: String -> String

compress "" = ""
compress [x] = [x]
compress (x:y:xs) 
  | x == y = compress (y:xs)
  | otherwise = x : compress (y:xs)
