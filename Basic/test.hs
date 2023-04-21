import Data.List (sort)
import System.IO
import System.Process -- clear terminal with exactly command : system "cls"
import Language.Haskell.TH (implBidir, nameBase)
import Data.Binary.Get (Decoder(Fail))
import Control.Concurrent.STM (check)
import Data.Tuple (swap)
import Data.Char (toLower, digitToInt, ord, isUpper)
import Data.Char

-- To clear terminal : using :! cls


-- -- Ugly, right?
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z


arr = 2 : 3 : []

myList = 1 : (2 : (3 : []))

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

evenList = [2,4..10]

many2s = take 5 (repeat 2) 
-- or 
many3s = replicate 5 3  -- [3,3,3,3,3]

cycleList = take 10 (cycle [1,2,3,4]) -- [1,2,3,4,1,2,3,4,1,2]

listTimes2 = [x * 2 | x <- [1..10]]

divisBy13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sumOfTwoLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

evenUpTo20 = takeWhile (<= 20) [2, 4..]


isOdd :: Int -> Bool
isOdd n
  | n `mod` 2 == 1 = True
  | otherwise = False

getItemList :: [Int] -> String

getItemList [] = "Your list is empty"  -- This is used for input an empty array 

getItemList (x:[]) = "Your list starts with " ++ show x -- This is used for input with array's length = 1
getItemList (x:y:[]) = "Your list starts with " ++ show x ++ "and " ++ show y -- This is used for input with array's length = 2
getItemList (x:xs) = "Your list starts with " ++ show x ++ " and the rest is " ++ show xs -- This is used for input with any array's length


getFirstList :: String -> String

getFirstList [] = "Your list is empty"
getFirstList all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

-- x : xs and map
times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4] -- [3,8,12,16]

multBy4 :: [Int] -> [Int]

multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs -- multBy4 [1,2,3] --> [4,8,12]

-- How it works
-- [1,2,3,4] = times 1 -> x = 1 and xs = [2,3,4]
-- [2,3,4] = times 2 -> x = 2 and xs = [3,4] --> so on until []



-- Pass function into function

doMult :: (Int -> Int) -> Int
doMult kha = kha 3 -- In which "kha" variable is a function. When you call doMult times4 (times4 is a previous function), the function times4 is put into "kha" and implement times4 3 which result in 12

getAddFunc :: Int -> (Int -> Int)

getAddFunc x y = x + y

num3 = getAddFunc 3

fourPlus3 = num3 4


-- Lambda 
dbl1to10 = map (\x -> x*2) [1..10]

-- If statement
checkAdult x = 
  if (x >= 18)
    then "Adult"
    else "Kid"


-- getClass :: Int -> String

getClass n = case n of 
  5 -> "Hello"
  6 -> "Hi"
  _ -> "go away"

-- Enumeration

data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfielder
                  deriving Show -- The Show typeclass defines how values of a data type should be represented as a string. By deriving an instance of Show for BaseballPlayer, you can use functions like show and print to convert values of the BaseballPlayer data type into a string representation
barry :: BaseballPlayer -> Bool
barry Outfielder = True 
barry _ = False

data Customer = Customer String String Double
            deriving Show 

vanKha :: Customer
vanKha = Customer "dep trai" "de thuong" 20.5

getBalance :: Customer -> Double
getBalance (Customer _ _ balance) = balance


-- Get input 
sayHello = do 
        putStrLn "what's your name"
        name <- getLine
        putStrLn $ "Hello " ++ name

-- File I/O import System.IO

writeToFile = do
      theFile <- openFile "text.txt" WriteMode
      hPutStrLn theFile ("Random line")
      hClose theFile

readFromFile = do
      theFile2 <- openFile "text.txt" ReadMode
      contents <- hGetContents theFile2
      putStr contents
      hClose theFile2

splitArr :: Int -> [a] -> ([a], [a])

splitArr n xs = (take n xs, drop n xs)


{- Define a function third :: [a] -> a that returns the third element in a list that contains at least
this many elements using:
a. head and tail;
b. list indexing !!;
c. pattern matching -}

-- Using head and tail
third :: [a] -> a

third xs = head (reverse (take 3 xs))

third1 :: [a] -> a
third1 xs = xs !! 2

third2 :: [a] -> a
third2 (_:_:x:_) = x

---------------------
grid :: Int->Int->[(Int, Int)]
grid x y = [(a, b) | a <- [0..x], b <- [0..y]]

square :: Int ->[(Int,Int)]
square x = [(a, b) | (a, b) <- grid x x, a /= b]

----------------------------
replicate' :: Ord a => Int->a->[a]
replicate' x a 
  | x == 0 = []
  | otherwise = a : replicate' (x-1) a

---------------------------------
scalarProduct :: [Int]->[Int]->Int

scalarProduct [] [] = 0
scalarProduct [x] [y] = x * y
scalarProduct (x:xs) (y:ys) = x * y + scalarProduct xs ys
----------------------------------
sumdown :: Int->Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

----------------------------------
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

----------------------------------
factors' n = [x | x <- [1..n - 1], n `mod` x == 0]
perfect' n = n > 0 && sum(factors' n) == n 
countPerfect :: [Int]->Int
countPerfect xs = length (filter perfect' xs)

----------------------------------
dec2int :: [Int] -> Int
dec2int xs = foldl (\acc x -> acc * 10 + x) 0 xs 


