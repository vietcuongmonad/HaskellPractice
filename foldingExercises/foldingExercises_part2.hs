{-Ex3: Calculate the interpolation polynomal in the Lagrange form
https://youtu.be/46dksIrx6jQ?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&t=365

lagrange [(1,2), (3,4)] 10 = 11

-}

-- Solution
lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x = foldl (\acc (xj, yj) -> acc + yj * l xj) 0 xs
    where l xj = foldl 
                    (\acc (x_element, _) -> 
                        if (xj == x_element) then acc 
                        else acc*(x-x_element)/(xj-x_element)) 
                    1 xs

-- My solution using foldr -> realise foldl & foldr mostly the same, only different is the order of the 'accmulator'
lagrange2 xs x = foldr (\(xj, yj) acc -> acc + yj * l xj) 0 xs
    where l xj = foldr
                    (\(x_element, _) acc -> 
                        if (xj == x_element) then acc 
                        else acc*(x-x_element)/(xj-x_element)) 
                    1 xs

{-Ex4: Create a function 'foldtrie' that folds the element of a trie
    in a 'preorder' traversal
    => It mean when we use a function f with 'foldtrie', it apply f in preorder
    
    preorder: root -> left -> right
-}
data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b

-- Solution
foldtrie f acc (Leaf x) = f acc x 
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs -- f acc x here means we traverse the root 1st
    where
        f' acc t = foldtrie f acc t -- traverse the remaining nodes


main :: IO()
main = do
    print $ (lagrange2 [(1,2), (3,4)] 10)
    print $ (lagrange2 [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)]) 5.0