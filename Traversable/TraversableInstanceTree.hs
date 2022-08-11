data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap f Leaf         = Leaf 
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node l v r) = foldMap f l `mappend` f v `mappend` foldMap f r
    
    foldr f acc Leaf = acc
    foldr f acc (Node l v r) = foldr f (f v (foldr f acc r)) l

instance Traversable Tree where
    traverse f Leaf         = pure Leaf
    traverse f (Node l v r) = Node <$> traverse f l <*> f v <*> traverse f r

main :: IO (Tree ())
main = do
    let myTree = Node (Node Leaf 'l' Leaf) 'v' (Node Leaf 'r' Leaf)
    traverse print myTree