import Control.Applicative

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop.doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

main :: IO ()
main = do
    print . bbop $ 3
    print . duwop $ 3