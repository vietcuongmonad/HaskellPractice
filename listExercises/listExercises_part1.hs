{-Ex1: Create a function elem_ that returns True if element is in a given list and return False otherwise -}
elem_ :: (Eq a) => a -> [a] -> Bool -- 'Eq a' <- is a type class; why 'elem_' because might be duplicate to 'elem'

-- # My solution
elem_ e [] = False
elem_ e (x:xs)
    | e == x    = True
    | otherwise = elem_ e xs

-- # Better solution
elem_sol :: (Eq a) => a -> [a] -> Bool
elem_sol _ [] = False
elem_sol e (x:xs) = (e == x) || (elem_sol e xs) 

{-Ex2: Create a function nub that removes all duplicate from a given list -}
nub :: (Eq a) => [a] -> [a]
nub [] = []
--nub (x:xs) = if (elem_sol x xs) then (nub xs) else (x: nub xs)
nub (x:xs)
    | (elem_sol x xs) = nub xs
    | otherwise       = x:nub xs

main :: IO()
-- main = print (elem_sol 5 [7, 6, 6, 4]) -- For ex1
main = print $ nub [2, 2]
