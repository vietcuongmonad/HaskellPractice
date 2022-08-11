--import Control.Monad.Trans.Writer (tell)
import Control.Monad.Writer
data MyWriter w a = Writer { runWriter :: (a, w) }

half :: Int -> MyWriter String Int
half x = do
    tell ("I just halved " ++ (show x) ++ "!")
    return (x `div` 2)