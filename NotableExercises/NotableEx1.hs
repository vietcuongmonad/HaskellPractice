{- Compute the perimeter of a polygon
https://www.hackerrank.com/challenges/lambda-march-compute-the-perimeter-of-a-polygon/problem?isFullScreen=true
 -}
dist (x,y) (u,v) = sqrt ((x-u)^2+(y-v)^2)

-- we use (acc, last_point) -> update acc then change last_point 
perimeter (x:xs) = fst $ foldr f (0, x) (x:xs)
    where f = \p (acc, q) -> (acc + (dist p q), p)

main = do
    n <- getLine
    all_input <- getContents
    let each_line = lines all_input
    let points_string = fmap words each_line
    -- convert to float so can use sqrt :: Float -> Float
    let points_float = fmap (\[a,b] -> (read a :: Float,read b :: Float)) points_string
    print . perimeter $ points_float
