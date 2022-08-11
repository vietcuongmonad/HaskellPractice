import Data.List (sort)

main :: IO ()
main = interact $ show . (/2) . cal . format . map read . tail . words

format :: [Double] -> [Double]
format (l:arr) = (l:sort arr)

cal :: [Double] -> Double
cal (l:x:xs) = max (x*2) $ cal_aux (l:x:xs)

cal_aux :: [Double] -> Double
cal_aux [l, x] = (l-x)*2
cal_aux (l:x:y:ys) = max (y-x) $ cal_aux (l:y:ys)