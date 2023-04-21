
rotateArray :: [Int] -> Int -> [Int]
rotateArray arr n = take len . drop (len - n `mod` len) $ arr ++ arr
  where len = length arr

-- How it works :
{- For example: given testcase [1,2,3,4,5] 3
   First step : Calculate len = length arr = 5
   Second step : Concentrate arr ++ arr = arr = [1,2,3,4,5,1,2,3,4,5]
   Third step : Calculate len - (n mod len) = 5 - (3 mod 5) = 2
   Fourth step : Drop (len - (n mod len) = 2) first elements -> arr = [3,4,5,1,2,3,4,5]
   Finally : Take len = 5 elements -> we get [3,4,5,1,2] 
 -}
