{-
Exercise: Write a program that read words we want to search from stdin then find those words in a file
If found, print "Found" else "Not Found"
More detail: https://youtu.be/WuGlElBjwVA?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&t=763
-}

import System.IO
import Data.List
import GHC.Unicode
import Data.Char

-- Through some research, but this is quite not natural
my_read_words :: IO [String]
my_read_words = do
    putStrLn "Specify the words to search"
    extra
    where
        extra = do
            putStr "> "
            text <- getLine
            if text == "" then return []
            else fmap (text:) extra

-- Author solution, more natural & easier to understand
sol_read_words :: IO [String]
sol_read_words = do
    putStrLn "Specify the words to search"
    aux
    where
        aux = do
            putStr "> "
            line <- getLine
            if (line == "") then return []
            else do
                xs <- aux
                return $ line:xs

my_read_file :: IO String
my_read_file = do
    putStr "File to search: "
    file <- getLine
    readFile file

lower :: [Char] -> [Char]
lower = map toLower

findString :: [String] -> String -> [String]
findString searchWords fileContent = [w | w <- searchWords, (lower w) `elem` modTextWords] -- elem check if an element exist
    where
        modTextWords = map lower $ words ftext
        ftext = filter (\x -> isLetter x || isSpace x) fileContent

main = do
    hSetBuffering stdout NoBuffering
    searchWords <- sol_read_words
    fileContent <- my_read_file
    let foundWords = findString searchWords fileContent
    let notFoundWords = [w | w <- searchWords, not (elem w foundWords)]
    mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found") foundWords
    mapM_ (\s -> putStrLn`` $ "\"" ++ s ++ "\" NOT found") notFoundWords
    