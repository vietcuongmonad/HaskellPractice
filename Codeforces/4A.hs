-- https://codeforces.com/problemset/problem/4/A

f :: Integral a => a -> [Char]
f x = if (rem (x-3) 2 > 0) then "YES" else "NO"

main :: IO ()
main = interact $ f . read

{- Explain:
    interact: (String -> String) -> IO ()
        ?> take a function that take stdin as arguments & output to stdout
    rem: a -> a -> a
        ?> remaining of operator div
-}
