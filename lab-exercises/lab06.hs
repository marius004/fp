data Fruct = Mar String Bool | Portocala String Int

ePortocalaDeSilicia :: Fruct -> Bool
ePortocalaDeSilicia (Portocala name _) = name `elem` ["Sanguinello", "Tarocco", "Moro"]
ePortocalaDeSilicia _ = False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia xs = 
    sum [b | Portocala a b <- xs, ePortocalaDeSilicia (Portocala a b)]

nrMereViermi :: [Fruct] -> Int
nrMereViermi xs = sum [1 | Mar _ b <- xs, b]

type NumeA = String
type Rasa = String
data Animal =  Pisica NumeA | Caine NumeA Rasa 
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String  
rasa (Caine _ rasa) = Just(rasa)
rasa (Pisica _) = Nothing

data Linie = L [Int] deriving Show
data Matrice = M [Linie] deriving Show

verifica :: Matrice -> Int -> Bool 
verifica (M xs) n =
    (length $ filter (\(L line) -> sum line /= n) xs) == 0

doarPozN :: Matrice -> Int -> Bool 
doarPozN (M xs) n 
    = foldl (\acc (L line) -> if length line == n then allPositive line && acc else acc) True xs
    where allPositive xs = (length $ filter (\x -> x < 0) xs ) == 0

corect :: Matrice -> Bool
corect (M xs) 
    | length xs <= 1 = True
    | otherwise = let lens = [length line | (L line) <- xs] 
                    in foldl (\acc x -> acc && x == head lens) True lens 