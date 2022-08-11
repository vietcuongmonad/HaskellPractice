import Data.List

main :: IO ()
main = interact $ unwords . map show . solve . map read . tail . words

solve :: [Int] -> [Int]
solve arr = let (smal, big) = (minimum arr, maximum arr)
                p = length . filter (==smal) $ arr in
    if (smal < big) then let q = length . filter (==big) $ arr
        in [big-smal, p*q]
    else [0, div (p*(p-1)) 2]