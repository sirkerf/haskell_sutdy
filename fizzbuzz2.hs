--import Control.Applicative ((<*>))
import Control.Monad (guard)
--import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [1..100]

fizzbuzz :: Integer -> String
fizzbuzz = let (d ~> s) n = s <$ guard (n `mod` d == 0)
             in fromMaybe . show <*> 3 ~> "Fizz" <> 5 ~> "Buzz"