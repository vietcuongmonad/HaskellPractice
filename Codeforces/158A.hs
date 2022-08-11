-- https://codeforces.com/problemset/problem/158/A

main :: IO ()
main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve (_:k:arr) = let tmp = max 0 (arr!!(k-1)-1)
    in sum[1 | x <- arr, x > tmp]