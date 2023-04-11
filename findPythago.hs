
-- Find Pythago
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x,y,z) | (x, y, z) <- combinations a, x^2 + y^2 == z^2]

combinations :: Int -> [(Int, Int, Int)]
combinations a = [(x, y, z ) | x <- [1..a], y <- [1..a], z <- [1..a]]
