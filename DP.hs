import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as DL
import qualified Data.Char as DC
import qualified Control.Monad as CM

q1' :: Int -> Int -> [(Int, Int)] -> Int
q1' n m wvs =runST $ do
    dp <- newArray ((0, 0), (n, m)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ [1 .. n] $ \i -> do
        let (w,v) = wva ! i
        forM_ [0 .. m] $ \j -> do
            x1 <- readArray dp (i-1, j)
            if j < w
                then writeArray dp (i,j) x1
            else do
                x2 <- readArray dp (i-1,j-w)
                writeArray dp (i,j) (max x1 (x2+v))
    readArray dp (n,m)
 where
    wva = listArray (1,n) wvs

getintlist = DL.unfoldr (BS.readInt . BS.dropWhile DC.isSpace) <$> BS.getLine

main :: IO()
main = do
    [n,maxW] <- getintlist
    items <- CM.replicateM n $ do
        [w,v] <- getintlist
        return (w,v)
    print $ q1' n maxW items