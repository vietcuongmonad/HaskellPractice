{- Ex 1. Write an IO program which will first read a positive integer, say n, 
    and then reads n integers after, each number seperate by space and then
    writes their sum. -}

-- # My Solution, after line 'n' is the line of 'n' integer
import System.IO

toInt :: String->Int
toInt = read::String->Int

sum_n = do
    n <- getLine
    putStrLn ("n= " ++ n)
    st <- fmap words getContents
    let tmp = fmap toInt st
    putStrLn ("The list= " ++ (show tmp))
    putStrLn ("Sum= " ++ (show (sum tmp)))

-- # Author solution, but this is after 'n', each n line has a number

prompt p = do 
    putStr p
    hFlush stdout    -- imported from System.IO

sumOfNumbers =
  do putStrLn "Compute the sum fo some numbers."
     prompt "How many numbers? "
     n <- readLn
     let ask n = do prompt ("Enter a number: ")
                    readLn
     ns <- mapM ask [1..n]
     print ns
     putStr "The sum of the numbers is "
     print (sum ns)

main :: IO()
main = do
    sum_n