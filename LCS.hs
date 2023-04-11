-- Write a function to find the longest common subsequence of two strings using recursion.
lcs :: String -> String -> String

lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys) 
  | x == y = x : lcs xs ys
  | otherwise = let lcs1 = lcs (x:xs) ys
                    lcs2 = lcs xs (y:ys)
                in if length lcs1 > length lcs2 then lcs1 else lcs2 -- let...in... define local varibles that are only accessible within the in part of the expression.The syntax let bindings in expression where bindings is a list of one or more varible definitions and expression is an expression that can use the varibles defined in bindings. 
