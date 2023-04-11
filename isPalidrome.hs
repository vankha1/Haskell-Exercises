
-- Check palidrome
compareList :: Eq a => [a] -> [a] -> Bool
compareList [] [] = False
compareList [x] [y] = x == y
compareList (x:xs) (y:ys) = x == y && compareList xs ys
compareList _ _ = False

checkPalidrome :: Eq a => [a] -> Bool
checkPalidrome [] = True
checkPalidrome [x] = True
checkPalidrome xs = compareList xs (reverse xs)
-- Another solution

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = head xs == last xs && isPalindrome (init (tail xs))


-- Check string if being palidrome 
palidromeString :: String -> Bool

palidromeString "" = True
palidromeString str = str == reverse str
