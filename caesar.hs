import Data.Char 

caesar :: [Char]->Int->[Char]
caesar [] _ = []
caesar (x:xs) n 
  | isLower x =  chr ((ord x - ord 'a' + n) `mod` 26 + ord 'a')  : caesar xs n
  | isUpper x =  chr ((ord x - ord 'A' + n) `mod` 26 + ord 'A')  : caesar xs n
  | otherwise = x : caesar xs n

