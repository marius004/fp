```hs
fmap f (Just a) = Just (f a)

half x = if even x then Just(x `div` 2) else Nothing

-- half 4 => 4
-- half (Just 4) => error
-- (Just 4) >>= half => Just 2 

data Maybe2 a = Just2 a | Nothing2 deriving Show

-- (>>=) :: m a -> (a -> m b) -> m b

{- class Monad m where 
    -- bind
    (>>=) :: m a -> (a -> m b) -> m b
    -- "m" = monad
    -- m a = (Just2 4)
    -- (a -> m b) = half

 -}

instance Monad Maybe2 where
    Nothing2 >>= f = Nothing2
    Just2 >>= f = f val -- half val -> Just2

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Tip a) = Tip (f a)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r) 

instance Applicative Tree where
    pure = Tip
    Tip f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

g x | x == 4 (Tip 99) | otherwise = Branch (Tip (x * 2) Tip (x * 4))

instance Monad Tree where 
    Tip a >>= f = f a
    Tip left right >>= f = Branch (left >>= f) (right >>= f)

```