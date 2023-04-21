-- import Data.Tuple
myLast :: [a] -> a
myLast xs = last xs

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Don't have butLast"
myButLast xs = 
    let temp = take (length xs - 1) xs
    in last temp

elementAt :: Ord a => [a] -> Int -> a
elementAt xs x = xs !! (x-1)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)


compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)


pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (== x) xs
              in (x:first) : pack rest
        
-- span take a predicate and a list, and return a tuple containing two lists. The first list contains the longest prefix of the input of list for which all ele satisfies the predicate. The second is the remaining list.


encode :: Eq a =>  [a] -> [(Int, a)]
encode [] = []
encode (x : xs) = let (first, rest) = span (== x) xs
    in (length first + 1, x) : encode rest


