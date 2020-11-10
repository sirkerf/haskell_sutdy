import Control.Monad

main = do
	putStrLn "いくつまで？: "
	numStr <- getLine
	let num = read numStr
	fizzbuzz num

fizzbuzz num = do
	forM_ [1..num] $ \i -> do
		putStrLn (show i ++ ": " ++ toFizzBuzz i)

toFizzBuzz num =
	case mod num 15 of
		0 -> "FizzBuzz"
		3 -> "Fizz"
		5 -> "Buzz"
		6 -> "Fizz"
		9 -> "Fizz"
		10 -> "Buzz"
		12 -> "Fizz"
		_ -> ""