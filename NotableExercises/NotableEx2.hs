{-Write a program that cut an infinity tree at certain height
    (l,r)
    /   \
(l+1,r) (l,r+1)
Source: https://youtu.be/WuGlElBjwVA?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&t=167
-}
data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Author's description
inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = aux (0,0)
    where
        aux (l,r) = Node (aux $ (l+1,r)) (l,r) (aux $ (l,r+1))

-- My way of showing the tree in pre-order
preorder:: Tree a -> [a]
preorder Leaf = []
preorder (Node left_child root right_child) = root : preorder left_child ++ preorder right_child

-- My solution
cut1 :: Integer -> Tree a -> [a]
cut1 h (Node left_child root right_child) =
    if (h == 0) then []
    else root : (cut1 (h-1) left_child) ++ (cut1 (h-1) right_child)

-- Author's solution
cut_sol :: Integer -> Tree a -> Tree a
cut_sol 0 _    = Leaf 
cut_sol h Leaf = Leaf
cut_sol h (Node l v r) = Node (cut_sol (h-1) l) v (cut_sol (h-1) r)

main = do
    print . preorder $ cut_sol 2 inv_tup_tree