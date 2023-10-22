import Data.Char (digitToInt)

vocale :: String -> Int
vocale [] = 0
vocale (x:xs)
    | x `elem` "aeiouAEIOU" = 1 + vocale xs 
    | otherwise = vocale xs

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (x:xs) = vocale x + nrVocale xs

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (x:xs)
    | even x = x : n : f n xs
    | otherwise = x : f n xs

diviz :: Int -> Int -> [Int]
diviz i n
    | i > n = []
    | mod n i == 0 = i : diviz (i + 1) n
    | otherwise = diviz (i + 1) n

divizori :: Int -> [Int]
divizori 0 = []
divizori n = diviz 1 n

listadiv :: [Int] -> [[Int]]
listadiv [] = []
listadiv (x:xs) = divizori x : listadiv xs

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b xs = [x | x <- xs, x >= a && x <= b]

pozitii :: Int -> [Int] -> [Int]
pozitii _ [] = []
pozitii i (x:xs)
    | mod x 2 == 1 = x: pozitii (i + 1) xs 
    | otherwise = pozitii (i + 1) xs

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare [] = []
pozitiiImpare xs = pozitii 0 xs

-- pozitiiImpare :: [Int] -> [Int]
-- pozitiiImpare xs = [i | (x, i) <- zip xs [1..], odd i]

multDigits :: String -> Int
multDigits = foldr (\x acc -> if x `elem` "0123456789" then acc * digitToInt x else acc) 1
