import Data.Bits ( popCount )

main :: IO()
main = interact $ show . (popCount::Int->Int) . read