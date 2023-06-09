encodeModified  :: Eq a =>  [a] -> [(String, Int, a)]
encodeModified  [] = []
encodeModified  (x : xs) = let (first, rest) = span (== x) xs
    in if (length first > 1) then ("Multiple",length first + 1, x) : encodeModified rest else ("Single",length first + 1, x) : encodeModified rest



data ListItem a = Single a | Multiple Int a
    deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeHelper x ++ decodeModified xs
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x