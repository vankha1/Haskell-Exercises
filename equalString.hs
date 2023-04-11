-- Check two string if being equal

areStringEqual :: [Char] -> [Char] -> Bool

areStringEqual [] [] = True
areStringEqual (x:xs) (y:ys) = x == y && areStringEqual xs ys
areStringEqual _ _ = False --If the input arguments that do not match the patterns in the previous line --> return false