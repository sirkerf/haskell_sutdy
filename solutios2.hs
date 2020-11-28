myButLast :: [a] -> a
myButLast = last . init

myButLast' x = reverse x !! 1

myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs

myButLast''' (x:(_:[])) = x
