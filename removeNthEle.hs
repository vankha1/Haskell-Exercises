-- Remove nth element from a list using recursion
removeEle :: [a]-> Int -> [a]

removeEle [] x = []
removeEle [x] 0 = []
removeEle (x:xs) n 
  | n == 0 = xs
  | otherwise = x : removeEle xs (n-1)