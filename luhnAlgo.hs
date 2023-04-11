-- luhn algorithms

luhnDouble :: Int->Int
luhnDouble x = if x > 9 then x*2 - 9 else x*2

luhn :: Int->Int->Int->Int->Bool

luhn x y z t = total `mod` 10 == 0
  where total = sum [luhnDouble x, y, luhnDouble z, t]
        
