halve :: Integral a => a -> Maybe a
halve x 
    | even    x = Just (x `div` 2)
    | otherwise = Nothing

oneFourth :: Integral b => b -> Maybe b
oneFourth x = halve x >>= halve

