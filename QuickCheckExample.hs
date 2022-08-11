import Test.QuickCheck

rev xs = rev_aux xs []
    where
        rev_aux [] acc     = acc
        rev_aux (x:xs) acc = rev_aux xs (x:acc)

propRev :: [Int] -> Property 
propRev xs = rev xs === reverse xs 

main :: IO()
main = quickCheck (verbose propRev)