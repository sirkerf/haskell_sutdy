foldl :: (b -> a -> b) -> b -> [a] -> b
readHex'' :: String -> Int
readHex'' str = foldl go 0 str
 where
     go :: Int -> Char -> Int
     go a x = a * 10 + digitToInr x