
scytale :: [Char] -> Int -> [Char]
scytale text columns = [text !! i | j <- [0..columns-1], i <- [j, j+columns..length text - 1]]
