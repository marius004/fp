factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0]

prim :: Int -> Bool 
prim nr = (length $ factori nr) == 2

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 x y z = [(a, b, c) | ((a, b), c) <- zip (zip x y) z]

firstElements :: [(a, b)] -> [a]
firstElements xs = map (\(a, _) -> a) xs

sumList :: [[Int]] -> [Int]
sumList l = map (\xs -> sum xs) l 

prel2 :: [Int] -> [Int]
prel2 xs = map (\nr -> if even nr then nr `div` 2 else nr * 2) xs

containsChar :: Char -> [String] -> [String]
containsChar ch xs = filter (elem ch) xs

patrateImpare :: [Integer] ->  [Integer]
patrateImpare xs = map (^2) $ filter odd xs

pozitiiImpareLista :: [Int] -> [Int]
pozitiiImpareLista xs = 
    map (^2) $
    map(\(_,b) -> b) $
    filter (\(a, _) -> odd a) ys 
    where ys = zip [1..] xs

numaiVocale :: [String] -> [String]
numaiVocale xs = map (\str -> filter (`elem` "aeiouAEIOU") str ) xs
