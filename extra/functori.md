```hs
{- A functor applies a function to a wrapped value and gives us a wrapped value back. -}

class Functor f where
    fmap :: (a -> b) -> f a -> f b

{-
fmap length (Just [1..5])

f = Just 
fmap :: ([] -> Integer) -> (Just []) -> (Just Integer)
f = contextul in care e wrapped

instance Functor (Either e) where 
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

fmap :: (a -> b) -> Either e a -> Either e b


Either String
(a -> b) => Int -> Int

fmap (Int -> Int) -> Either String Int -> Either String Int
fmap _ (Left str) = Left str
fmap f (Right i) = Right(f i)

-}

Stim ca const :: a -> b -> a. Fie replaceWithP = const 'p'. Ce se obtine dupa fmap replaceWithP (Just 10)?

instance Functor Maybe 10 where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just(f a)
```

fmap replaceWithP (Just 10)
       (b -> a)
      (Num -> [Char])

Deci da (Just 'p')

data Arbore a = Nil | Nood a (Arbore a) (Arbore a)
instance Functor Arbore where 
    fmap f Nil = Nil
    fmap f (Nod x l r) = Nod (f x) (fmap f l) (fmap f r)