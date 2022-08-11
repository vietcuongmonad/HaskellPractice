half :: (Integral a, Show a) => a -> (a, [Char])
half x = (x `div` 2, "Just half " ++ show (x) ++ "! ")

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = (f x) in (y, log `mappend` newLog)

main :: IO ()
main = do
    print . half $ 10
    print $ (3, "asdf ") `applyLog` half