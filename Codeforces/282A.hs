main :: IO ()
main = do
    n <- readLn
    interact $ (\exp -> show $ sum [1 | '+' <- exp] - n)