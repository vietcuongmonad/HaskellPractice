cal :: Integral a => [a] -> a
cal [n,m,k] = (div (-n) k) * (div (-m) k)

main :: IO()
main = interact$ show. cal. map read. words