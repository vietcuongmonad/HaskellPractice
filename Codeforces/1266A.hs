import Data.Char ( digitToInt )

main :: IO ()
main = interact $ unlines . map solve . tail . words

solve :: [Char] -> [Char]
solve exp
    | not . elem 0 $ digits                = "cyan"
    | (/= 0) . (`mod` 3) . sum $ digits    = "cyan"
    | (<2) . length . filter even $ digits = "cyan"
    | otherwise                            = "red"
    where digits = map digitToInt exp