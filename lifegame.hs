import Control.Monad
import System.Random

type Pos = (Int, Int)
type Board = [Pos]

width :: Int
width = 50

height :: Int
height = 50

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

wrap :: Pos -> Pos
wrap (x, y) =(((x-1) `mod` width) + 1,
              ((y-1) `mod` height)+ 1)

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [(x-1, y-1), (x, y-1),
                             (x+1, y-1), (x-1, y),
                             (x+1, y),   (x-1, y+1),
                             (x+1, y),   (x-1, y+1),
                             (x, y+1),   (x+1, y+1)]

liveneighbors :: Board -> Pos -> Int
liveneighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [p | p <- b , liveneighbors b p `elem` [2, 3]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

births :: Board -> [Pos]
births b = [p | p <- rmdups (concatMap neighbors b),
                isEmpty b p,
                liveneighbors b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return()
seqn (a:as) = do a
                 seqn as

showcells :: Board -> IO ()
showcells b = seqn [writeat p "0" | p <- b]

--wait :: Integer -> IO a0

lifegame :: Board -> IO ()
lifegame b = do cls
                showcells b
                --wait 5000
                lifegame (nextgen b)

makeLife :: IO Pos
makeLife = do
       x <- getStdRandom $ randomR(1, width) :: IO Int
       y <- getStdRandom $ randomR(1, height) :: IO Int
       return (x, y)

initBoard :: IO Board
initBoard = replicateM (width * height `div` 5) makeLife

main :: IO ()
main = lifegame =<< initBoard