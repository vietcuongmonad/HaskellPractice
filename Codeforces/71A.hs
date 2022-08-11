-- https://codeforces.com/problemset/problem/71/A
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

abbreviate :: [Char] -> [Char]
abbreviate st = let n = length st in
    if n > 10 then (head st):(show (n-2)) ++ [last st]
        else st

main :: IO ()
main = interact $ unlines . tail . map abbreviate . lines