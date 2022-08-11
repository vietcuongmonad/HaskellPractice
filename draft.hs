main :: IO()
main = do
    line <- getLine
    let w = (read :: String -> Integer) line
    if (w>2 && even w) then putStr "YES"
    else putStr "NO"


