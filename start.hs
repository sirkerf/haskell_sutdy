import Data.Char
main = do
    s <- getLine
    case filter (not <$> isDigit) s of
        [] -> print $ 2 * read s
        otherwise -> putStrLn "error"