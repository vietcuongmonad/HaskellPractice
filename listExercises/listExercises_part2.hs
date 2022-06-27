{-Ex3. Create a function 'isAsc' that returns True if the list given to is ascending order-}
isAsc :: [Int] -> Bool

-- # My solution -> Wrong, what about case only 1 elements like [x]
isAsc [] = True
isAsc [x, y] = True
isAsc (x:y:ys)
    | x > y    = False
    | otherwise = isAsc (y:ys)

-- # Better solution
isAsc_sol [] = True
isAsc_sol [x] = True
isAsc_sol (x:y:ys) = (x <= y) && isAsc_sol (y:ys)

{-Ex4. Create a function 'hasPath' that determines if a path from 1 node to another exists within a directed graph-}
hasPath :: [(Int, Int)] -> Int -> Int -> Bool

-- # My solution -> Even though this works, for big graph, it will be big complexity
hasPath [] u v = (u == v)
hasPath ((x, y):the_rest) u v
    | u == v                                           = True
    | u == x && v == y                                 = True
    | (hasPath the_rest u x) && (hasPath the_rest y v) = True
    | otherwise                                        = hasPath the_rest u v

-- # Better solution
hasPath_sol [] u v = (u == v)
hasPath_sol graph u v
    | u == v     = True
    | otherwise  = let graph' = [(x, y) | (x, y) <- graph, u /= x] in 
        (or [ hasPath_sol graph' y v | (x, y) <- graph, u == x]) 
    -- This is actually like DFS, when you find all the nodes that adjacent to u (graph'), then from there dfs to v

main :: IO()
main = print $ hasPath_sol [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)] 1 3