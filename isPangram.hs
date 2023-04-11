-- Write a function to determine if a string is a pangram (contains every letter of the alphabet) using recursion.
import Data.Char (toLower, digitToInt, ord, isUpper)

isPangram :: String -> Bool
isPangram s = isPangram' s ['a'..'z']
  where
    isPangram' _ [] = True
    isPangram' [] _ = False
    isPangram' (x:xs) alphabet = isPangram' xs (filter (/= toLower x) alphabet)