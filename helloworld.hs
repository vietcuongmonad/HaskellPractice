manual_and :: [Bool] -> Bool
manual_and [] = True
manual_and (x:xs)
    | x == False = False
    | otherwise = manual_and xs

-- Write a higher order function that apply a function increment a value to 1
app :: (a -> b) -> a -> b
app f x = f x

inc :: Integer -> Integer
inc x = x + 1


main :: IO()
main = print (map (\(x, y) -> x+y) [(1, 2), (2, 3), (3, 4)])