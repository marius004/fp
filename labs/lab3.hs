import Data.Char (isDigit, digitToInt)

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs)
    | x == reverse x = nrVocale xs + vocale x
    | otherwise = nrVocale xs
    where vocale str = foldr (\x acc -> if x `elem` "aeiouAEIOU" then acc + 1 else acc) 0 str

f :: Int -> [Int] -> [Int]
f _ [] = []
f nr (x:xs) 
    | even x = x : nr : f nr xs
    | otherwise = x : f nr xs 

divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], n `mod` x == 0]

listadiv :: [Int] -> [[Int]]
listadiv xs = [divizori x | x <- xs]

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b xs = [x | x <- xs, x >= a && x <= b]

pozitive :: [Int] -> Int 
pozitive xs = length [x | x <- xs, x > 0]

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare xs = [b | (a, b) <- zip [1..] xs, odd a]

multDigits :: String -> Int
multDigits xs = product [digitToInt x | x <- xs, isDigit x]

