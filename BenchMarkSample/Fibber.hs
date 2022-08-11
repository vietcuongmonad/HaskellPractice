import Criterion.Main

fib m | m < 0 = error "negative!"
      | otherwise = fib_aux m 
  where
      fib_aux 0 = 0
      fib_aux 1 = 1
      fib_aux m = fib_aux (m-1) + fib_aux (m-2)

main = defaultMain [
    bgroup "fib" [ bench "1"  $ whnf fib 1
                 , bench "5"  $ whnf fib 5
                 , bench "9"  $ whnf fib 9
                 , bench "11" $ whnf fib 11
                 ]
    ]


