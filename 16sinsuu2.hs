readHex' :: [Char] -> Int
readHex' str = go 0 str
 where
     go :: Int -> [Char] ->Int
     go a xxs = case xxs of
         x : xs -> go (a * 10 + digitToInt x) xs
         [] -> a