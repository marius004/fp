poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*(x^2)+b*x+c

eeny :: Int -> String 
eeny x
    | even x = "eeny"
    | otherwise = "meeny"

shit :: Int -> Int
shit x 
    | mod x 3 == 0 = 1
    | mod x 5 == 0 = 2
    | otherwise = 3

binomial :: Int -> Int -> Int 
binomial 0 _ = 0
binomial n 0 = 1
binomial n k = 
    binomial (n-1) k + binomial (n - 1) (k - 1)

verifL :: [Int] -> Bool 
verifL xs = mod (length xs) 2 == 0

takefinal :: [Int] -> Int -> [Int]
takefinal xs n 
    | length (xs) < n = xs 
    | otherwise = drop ((length xs) - n) xs

remove :: [Int] -> Int -> [Int]
remove xs n 
    | length (xs) < n = xs 
    | otherwise = (take (n - 1) xs) ++ (drop n xs)

myreplicate :: Int -> Int -> [Int]
myreplicate n v = [v | _ <- [1..n]]

sumImp :: [Int] -> Int 
sumImp xs = sum [x | x <- xs, odd x]

totalLen :: [String] -> Int 
totalLen xs = sum [length x | x <- xs, head x == 'a']