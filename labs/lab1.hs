fact :: Integer -> Integer
fact n 
    | n <= 0 = 1
    | otherwise = fact(n-1) * n

maxi :: [Integer] -> Integer 
maxi [a] = a
maxi (x:xs) =
    let 
        xs_max = maxi(xs)
    in 
        if x > xs_max 
            then x 
            else xs_max 