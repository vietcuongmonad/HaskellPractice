{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
main :: IO ()
main = interact $ unlines . map solve . format . map read . tail . words

format :: [Int] -> [[Int]]
format allTest = format_aux allTest []
    where
        format_aux []         res = res
        format_aux (n:m:rest) res = let (arr, remain) = splitAt n rest in
            format_aux remain (res ++ [[n, m] ++ arr])

solve :: [Int] -> [Char]
solve [] = []
solve (n:m:arr) = let st = take m ['B', 'B'..] in
    solve_aux arr st where
        solve_aux []     cur_st = cur_st
        solve_aux (x:xs) cur_st = let (smal, big) = judge (x-1) (m-x) in
            if (cur_st !! smal == 'B') then solve_aux xs (change smal cur_st)
                else solve_aux xs (change big cur_st) 

judge :: Int -> Int -> (Int, Int)
judge x y = if (x < y) then (x, y) else (y, x)

change :: Int -> [Char] -> [Char]
change pos st = let (xs,_:ys) = splitAt pos st in
    xs ++ 'A':ys