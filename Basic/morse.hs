morse :: [Char] -> [Char]
morse [] = []
morse (x:xs) = case x of
    'A' -> ".- " ++ morse xs
    'B' -> "-... " ++ morse xs
    'C' -> "-.-. " ++ morse xs