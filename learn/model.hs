{- 
I
1) a
2) c 
3) a
4) a
5)
6)
7)
8)
9) a
10) a
-}

{-
ii
Sa se scrie o functie care primeste ca argumente doua siruri de caractere, si
afiseaza cel mai lung prefix comun.
f “sirulnr1” “sirdoi” = “sir”
-}

prefix :: [Char] -> [Char] -> [Char]
prefix (x:xs) (y:ys) 
    | x == y = x:(prefix xs ys)
    | otherwise = []

{-
Sa se scrie o functie care pentru doua liste, x si y, calculeaza suma produselor
xi^2 * yi^2 cu xi din x si yi din y. Daca listele au lungimi diferite, functia va
arunca o eroare.
f [1,2,3,4] [5,6,7,8] == (1^2 * 5^2) + (2^2*6^2) + (3^2*7^2)
-}

f :: Num a => [a] -> [a] -> a
f x y 
    | length x /= length y = error "lungimi diferite"
    | otherwise = sum [x*x*y*y | (x, y) <- zip x y]

-- iii

data PairInt = P Int Int deriving Show
data MyList = L [PairInt] deriving Show
data Exp = I Int | Add Exp Exp | Mul Exp Exp deriving Show

class MyClass m where
    toExp :: m -> Exp

{- 
Sa se scrie o instanta a clasei MyClass pentru tipul MyList astfel incat functia toExp
sa transforme o lista de perechi astfel: o pereche devine adunare intre cele doua
elemente, iar intre elementele listei se aplica operatia de inmultire. Pentru lista vida
puteti considera ca expresia corespunzatoare este I 1.
-}

instance MyClass MyList where 
    toExp (L pairs) = 
        foldr(\(P a b) acc -> Mul (Add (I a) (I b)) acc) (I 1) pairs

eval :: MyList -> Int
eval list = evalExp $ toExp list

evalExp :: Exp -> Int
evalExp (I a) = a
evalExp (Add exp1 exp2) = evalExp exp1 + evalExp exp2
evalExp (Mul exp1 exp2) = evalExp exp1 * evalExp exp2 