module Multiply.Bug.Fix where

multiply ::Int -> Int -> Int
multiply a b = a * b

main :: IO()
main = do
    [a,b] <- map read . words <$> getLine
    print $ multiply a b