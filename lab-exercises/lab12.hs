import Data.Maybe (isNothing)

{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)

appendList :: List a -> List a -> List a
appendList Nil ys = ys
appendList (Cons x xs) ys = Cons x (appendList xs ys)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil 
    Cons f fs <*> xs = appendList (fmap f xs) (fs <*> xs)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = 
        Cow {
            name :: String
            , age :: Int
            , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty str = if str == "" then Nothing else Just str   

noNegative :: Int -> Maybe Int
noNegative nr = if nr < 0 then Nothing else Just nr  

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

{- cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight 
    | isNothing $ noNegative age = Nothing
    | isNothing $ noEmpty name   = Nothing
    | otherwise = Just Cow { name = name, age = age, weight = weight }
-}

cowFromString name age weight = 
    Cow <$> noEmpty name <*> noNegative age <*> pure weight

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength len str = if length str < len then Just str else Nothing 

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName name = fmap Name $ validateLength 25 name
-- mkName name = if isNothing $ validateLength 25 name then Nothing else Just (Name name) 

mkAddress :: String -> Maybe Address
mkAddress address = fmap Address $ validateLength 100 address
-- mkAddress adr = if isNothing $ validateLength 100 adr then Nothing else Just (Address adr)  

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson name address = Person <$> mkName name <*> mkAddress address

{- mkPerson name address 
    | isNothing $ mkName address = Nothing
    | isNothing $ mkAddress name   = Nothing
    | otherwise = Just (Person (Name name) (Address address))
 -}

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))