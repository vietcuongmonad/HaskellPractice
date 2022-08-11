main = interact $ show . locate allAns . map read . words 

allAns :: [Integer]
allAns = map (\x -> abs(div x 5 - 2)+abs(mod x 5 - 2)) [0..]

locate :: [Integer] -> [Integer] -> Integer
locate (x:xs) (0:ys) = locate xs ys
locate (x:_) _ = x