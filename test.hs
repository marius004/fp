f x = case x of 
    0 -> 0
    1 -> y + 1
    2 -> y * y
    _ -> y
    where y = x * x

-- (:) :: a -> [a] -> [a]
(&&&) :: Bool -> Bool -> Bool
True &&& b = b 
False &&& _ = False

foo1 :: (Int,Char,String) -> String
foo1 (a,b,c) = ""

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
-- :t compose [(+1), (^2)]
-- compose [(+1), (^2)] :: Num a -> a -> a

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op z [a1 , a2 , a3 , . . , an ] =
    a1 `op` ( a2 `op` ( a3 `op` ( . . . ( an `op` z ) . . . ) ) )

foldl :: (b −> a −> b) −> b −> [a] −> b
foldl op z [ a1 , a2 , a3 , . . . , an ] =
( . . . ( ( ( z `op` a1 ) `op` a2 ) `op` a3 ) . . . ) `op` an
-}

-- fold* (^) 2 [1..3]
-- foldl: (((2 ^ 1) ^ 2) ^ 3)
-- foldr: 1 ^ (2 ^ (3 ^ 2))

data List a = Nil | Cons a (List a) deriving Show 

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

data Shape = Circle Float | Rectangle Float Float
data Doggies a = Husky a | Mastiff a

type FirstName = String
type LastName = String 
type Age = Int
type Height = Float
type Phone = String

{-
data Person = Person FirstName LastName Age Height Phone
firstName :: Person -> FirstName 
firstName (Person firstName _ _ _ _) = firstName
-}

data Person = Person { firstName :: String,
    lastName :: String, 
    age :: Int, 
    height :: Int,
    phoneNumber :: String 
} deriving Show

nextYear :: Person -> Person
nextYear person = person { age = age person + 1 }