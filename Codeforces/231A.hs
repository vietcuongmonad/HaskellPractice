-- https://codeforces.com/problemset/problem/231/A

count :: [Int] -> Int
count [] = 0
count (a:b:c:d) = (if (a+b+c)>1 then 1 else 0) + count d

main :: IO ()
main = interact$ show. count . map read. tail. words 