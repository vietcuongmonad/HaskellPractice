{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
data Error i e 
    = EndOfInput -- Expected more input, but there is nothing 
    | Unexpected i -- Didn't expect to find this element
    | Customer e -- Extra errors user may want to create
    | Empty 
    deriving (Eq, Show)

{-  i: input stream
    e: type of custom error messages
    a: result of our parsing function
-}
newtype Parser i e a = Parser {
    runParser :: [i] -> Either [Error i a] (a, [i])
}

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
    case input of
        [] -> Left [EndOfInput]
        hd:rest 
            | predicate hd -> Right (hd, rest)
            | otherwise    -> Left [Unexpected hd]

judge :: Eq i => i -> Parser i e i
judge x = satisfy (==x)