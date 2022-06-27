-- Ex1: Create a function rev that reverses a list
rev :: [a] -> [a]

-- #Remember: foldl f [1,2,3] z = ((1 f z) f 2) f 3
rev = foldl (\acc x -> x:acc) []
--rev = foldl (flip (:)) [] --#Another way: flip change order of arguments

{- Ex2: Create a function prefixes that returns all prefixes of a given list -}
-- # My solution number 1: Hoi cong kenh
add_end [] a = [a]
add_end (x:xs) a = x:(add_end xs a)

prefixes_tmp :: [a] -> [[a]] -> [a] -> [[a]]
prefixes_tmp acc res [] = res
prefixes_tmp acc res (x:xs) = let new_acc = add_end acc x
    in (prefixes_tmp new_acc (add_end res new_acc) xs)

prefixes_mysol1 :: [a] -> [[a]]
prefixes_mysol1 = prefixes_tmp [] []

-- # My solution number 2, much better
func z [] = []
func z (x:xs) = (z:x):(func z xs)

upgrade_func z [] = [[z]]
upgrade_func z (x:xs) = [z]:(func z (x:xs))

prefixes_mysol2 :: [a] -> [[a]]
prefixes_mysol2 = foldr upgrade_func []

-- # Better solution

prefixes_sol :: [a] -> [[a]]
prefixes_sol = foldr (\x acc -> [x] : map ((:) x) acc) []
-- Notice that (:) here means prepend, not append
-- This solution is so clean

main :: IO()
--main = print $ (prefixes_mysol2 [3,6,7])
--main = print (prefixes_sol [1,2,3])