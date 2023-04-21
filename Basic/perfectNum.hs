
factors n = [x | x <- [1..n - 1], n `mod` x == 0]
perfect n = [x | x <- [1..n], sum(factors x) == x]