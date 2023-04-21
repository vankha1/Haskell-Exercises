-- Reverse a list using recursion
revList :: [a] -> [a]

revList [] = []
revList (x:xs) = revList xs ++ [x]