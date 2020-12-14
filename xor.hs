import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.List (unfoldr)
import Data.Bool (bool)
import Data.Bits

main :: IO ()
main = do
  let
    fromListN :: Int -> [Int] -> (Int, IO (VUM.IOVector Int))
    fromListN n xs = (offset, mv)
      where
        offset = until (n <=) (2 *) $ 1 :: Int
        mv = do
          v <- VUM.replicate (offset + n) 0 :: IO (VUM.IOVector Int)
          let
            f = mapM_ (uncurry (VUM.write v)) . zip [offset ..]
            next :: Int -> Maybe (Int, Int)
            next 2 = Nothing
            next i = Just (i', i')
              where
                i' = pred i :: Int
          f xs
          mapM_ (\i -> do x <- VUM.read v i; VUM.modify v (xor x) (i `div` 2)) $ unfoldr next (offset + n)
          return v
    modify :: Int -> VUM.IOVector Int -> Int -> Int -> IO ()
    modify offset v = f
      where
        f :: Int -> Int -> IO ()
        f x = go . (offset +)
          where
            go :: Int -> IO ()
            go 0 = return ()
            go i = do {VUM.modify v (x `xor`) i; go (i `div` 2)}
    query :: Int -> VUM.IOVector Int -> (Int, Int) -> IO Int
    query offset v = uncurry ((<*>) . (xor <$>)) . foldl step (return 0, return 0) . unfoldr next . ((,) <$> (offset +) . fst <*> (offset +) . snd)
      where
        step :: (IO Int, IO Int) -> (Int, Int) -> (IO Int, IO Int)
        step (accL, accR) (i, j) = (accL', accR')
          where
            accL' = xor <$> accL <*> VUM.read v i :: IO Int
            accR' = xor <$> VUM.read v j <*> accR :: IO Int
        next :: (Int, Int) -> Maybe ((Int, Int), (Int, Int))
        next (i, j) | i < j     = Just ((bool 0 i $ odd i, bool 0 (j - 1) $ odd j), (i', j'))
                    | otherwise = Nothing
          where
            i' = (i + 1) `div` 2 :: Int
            j' = j `div` 2 :: Int
  [n, q] <- map unsafeTextToInt . T.words <$> T.getLine :: IO [Int]
  as <- map unsafeTextToInt . T.words <$> T.getLine :: IO [Int]
  let
    (offset, mv) = fromListN n as :: (Int, IO (VUM.IOVector Int))
  v <- mv :: IO (VUM.IOVector Int)
  let
    f :: (Int, Int, Int) -> IO ()
    f (0, x, y) = modify offset v y x
    f (1, x, y) = query offset v (x, y) >>= print
  qs <- map (unsafeListToTriple . map unsafeTextToInt . T.words) . T.lines <$> T.getContents :: IO [(Int, Int, Int)]
  mapM_ f qs

unsafeListToTriple :: [Int] -> (Int, Int, Int)
unsafeListToTriple = (,,) <$> pred . head <*> pred . head . tail <*> head . tail . tail
{-# INLINE unsafeListToTriple #-}

unsafeTextToInt :: T.Text -> Int
unsafeTextToInt s = case T.signed T.decimal s of
  Right (n, _) -> n
{-# INLINE unsafeTextToInt #-}

