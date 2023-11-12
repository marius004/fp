import Data.List (permutations)
sumaPatrateImpare :: [Int] -> Int
sumaPatrateImpare xs = 
    let impare = filter odd xs 
    in foldl (\acc x -> acc + x ^ 2) 0 impare

allTrue :: [Bool] -> Bool 
allTrue xs = foldl (&&) True xs

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies predicate xs = foldl(\acc x -> predicate x && acc) True xs

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr predicate xs = 
    foldr (\x acc -> if predicate x then x:acc else acc) [] xs

listToInt :: [Integer] -> Integer
listToInt xs = foldl (\acc x -> acc * 10 + x) 0 xs

rmChar :: Char -> String -> String
rmChar chr str = foldl(\acc x -> if chr == x then acc else acc ++ [x]) "" str

rmCharsRec :: String -> String -> String
rmCharsRec chrs str = 
    foldl (\acc x -> if x `elem` chrs then acc else acc ++ [x]) "" str

myReverse :: [Int] -> [Int]
myReverse xs = foldr (\x acc -> acc ++ [x]) [] xs

myElem :: Int -> [Int] -> Bool 
myElem nr xs = (length $ filter (\x -> x == nr) xs) > 0

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip xs =
    let fst = foldl (\acc (a, _) -> acc ++ [a]) [] xs
        snd = foldl (\acc (_, b) -> acc ++ [b]) [] xs 
        in (fst, snd)

removeDups :: [Int] -> [Int]
removeDups [] = []
removeDups (x:xs) 
    | x `elem` xs = removeDups(xs)
    | otherwise = x : removeDups(xs)  

myIntersect :: [Int] -> [Int] -> [Int]
myIntersect xs ys =
    let zipped = [(x, y) | x <- removeDups xs, y <- removeDups ys]
    in doUnion zipped 
    where doUnion zipped = let common = filter(\(a, b) -> a == b) zipped in foldl (\acc (a, _) -> acc ++ [a]) [] common

myUnion :: [Int] -> [Int] -> [Int]
myUnion xs ys =
    foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) (removeDups xs) ys

myPermutation :: [a] -> [[a]]
myPermutation [] = [[]]
myPermutation (x:xs) = [zs | ys <- myPermutation xs, zs <- interleave x ys]
    where interleave :: a -> [a] -> [[a]]
          interleave y [] = [[y]]
          interleave y (z:zs') = (y:z:zs') : map (z:) (interleave y zs')
