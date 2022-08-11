dist (x,y) (u,v) = sqrt ((x-u)^2+(y-v)^2)

perimeter (x:xs) = fst (foldr f (0, x) (x:xs)) -- (acc, last_point)
    where f = \p (acc, q) -> (acc + (dist p q), p)

main = do
    n <- getLine
    all_input <- getContents
    let each_line = lines all_input
    let points_string = fmap words each_line
    -- convert to float so can use sqrt :: Float -> Float
    let points_float = fmap (\[a,b] -> (read a :: Float,read b :: Float)) points_string
    print . perimeter $ points_float
