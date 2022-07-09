{- 
Write functions 'insert' & 'inorder' for binary tree type.
Create QuickCheck property that checks if the inorder traversal of a tree build with the insert 
    function is sorted
-}

import Data.List hiding (insert)
import Test.QuickCheck

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- My solution
insert :: (Ord a) => a -> Tree a -> Tree a
insert num Leaf = Node Leaf num Leaf
insert num (Node l v r)
    | num > v   = Node l v (insert num r)
    | otherwise = Node (insert num l) v r 

-- My solution
inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l v r) = (inorder l)++v:(inorder r)

-- Author's solution, 'where types' like that I've never seen before
propCheck xs = sort xs === xs'
    where
        types = xs :: [Int]
        xs' = inorder $ foldr insert Leaf xs

main :: IO()
main = do
    let tmp = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node Leaf 5 (Node Leaf 6 Leaf))
    print . inorder $ insert 3 tmp
    quickCheck (verbose propCheck)
