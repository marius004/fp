```hs
{-
<$> = fmap

fmap (+3) (Just 8) => Just 11
Just (+1) <*> (Just 8)

-}

-- class Functor f => Applicative f where 
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

data Maybe2 a = Just2 a | Nothing2 deriving Show
instance Functor Maybe2 where
    fmap f (Just a) = Just (f a)
    fmap f Nothing = Nothing

instance Applicative Maybe2 where 
    pure = Just2
    -- Just2 f <*> (Just j) = Just2 (f j)
    Just2 f <*> j = fmap f j
    Nothing2 <*> j = Nothing2

{-
(*) <$> (Just 8) == Just (*8)
(*) <$> (Just 8) <*> (Just 2) => Just 16

(*2) <$> [1,2,3] => [2,4,6]
[(*2), (+3)] <*> [1,2,3] => [2,4,6,4,5,6]
-}

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

-- Branch (Tip 4) (Branch (Tip 5) (Tip 6)) <*> Tip (*4)
-- Branch (Tip 16) (Branch (Tip 20) (Tip))

{-
x = Branch (Tip 4) (Branch (Tip 5) (Tip 6))
Branch (Tip (+1)) (Branch (Tip (*2)) <*> x

=> Branch (Branch (Tip 5) (Branch (Tip 6) (Tip 7))) 
(Branch (Tip 8) (Branch (Tip 10) (Tip 12))) 
-}

instance Functor Tree where
    fmap f (Tip a) = Tip (f a)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r) 

instance Applicative Tree where
    pure = Tip
    Tip f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative (Either a) where 
    pure = Right
    Left e <*> _ = Left e
    Right f <*> x = fmap f x
```
