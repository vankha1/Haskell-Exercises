{- Write a function countWords that takes a string as input and returns the number of words in the string. -}
import Data.Char

countWords :: String -> Int
countWords = length . myWords

myWords :: String -> [String]
myWords s = case dropWhile isSpace s of
    "" -> []
    s' -> w : myWords s''
        where (w, s'') = break isSpace s'