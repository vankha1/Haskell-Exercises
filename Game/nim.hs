{- type Nimboard = [Int]

getInt :: IO Int
getInt = getLine >>= return . read

initNimBoard :: IO Nimboard
initNimBoard = do
    putStrLn "Enter the number of rows: "
    numRows <- getInt
    let getRow i = do
            putStrLn ("Enter the number of stones in row " ++ show i ++ ": ") 
            getInt
    sequence [ getRow i | i <- [1..numRows] ] 

printNimboards :: Nimboard -> IO ()
printNimboards board = sequence_ [printLine rowAmount | rowAmount <- board]
    where printLine rowAmount = putStrLn (replicate rowAmount '*')


notEmptyBoard :: Nimboard -> Bool
notEmptyBoard board = False `elem` fmap (== 0) board

removeFromRow :: Nimboard -> Int -> Int -> Nimboard
removeFromRow board row amount = if board !! row < amount 
    then error "The actual number of stones is smaller!"
    else [if i == row
            then board !! i - amount
            else board !! i
          | i <- [0..length board - 1]]
main = do
    nimBoard <- initNimBoard
    putStrLn "Initial board"
    printNimboards nimBoard
    let play turn board = do 
            putStrLn ""
            putStrLn ("Player " ++ show turn ++ "'s turn")
            putStrLn "How many stones"
            amount <- getInt
            putStrLn "Which row ?"
            row <- getInt
            let newBoard = removeFromRow board (row - 1) amount
            if (not . notEmptyBoard $ newBoard)
            then do 
                putStrLn ("Player " ++ show turn ++ " has won") 
                return ()
            else do
                putStrLn ""
                putStrLn "Current board: "
                printNimboards newBoard
                play (2 - (turn + 1) `mod` 2) newBoard
    play 1 nimBoard -}
import System.IO

type Board = [Int]

displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "Current board:"
    mapM_ (\(i, x) -> putStrLn $ "Row " ++ show i ++ ": " ++ replicate x '*') $ zip [1..] board

playerMove :: Board -> IO Board
playerMove board = do
    putStr "Enter the row number: "
    hFlush stdout
    row <- readLn
    putStr "Enter the number of stars to remove: "
    hFlush stdout
    stars <- readLn
    let newBoard = take (row - 1) board ++ [board !! (row - 1) - stars] ++ drop row board
    return newBoard

nim :: Board -> IO ()
nim board = do
    displayBoard board
    if all (== 0) board then
        putStrLn "You win!"
    else do
        newBoard <- playerMove board
        nim newBoard

main :: IO ()
main = do
    let initialBoard = [5, 4, 3, 2, 1]
    nim initialBoard