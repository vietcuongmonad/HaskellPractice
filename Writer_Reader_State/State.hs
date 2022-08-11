import Control.Monad.State

greeter :: State String String
greeter = do 
    name <- get 
    put "tintin"
    return ("hello, " ++ name ++ "!")