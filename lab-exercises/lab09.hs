{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

data Tree = Empty  | Node Int Tree Tree Tree 

class ArbInfo t where
  level :: t -> Int
  sumval :: t -> Int
  nrFrunze :: t -> Int 

instance ArbInfo Tree where 
    level Empty = 0
    level (Node _ s1 s2 s3) = 1 + max (max (level s1) (level s2)) (level s3)

    sumval Empty = 0
    sumval (Node x s1 s2 s3) = x + sumval s1 + sumval s2 + sumval s3

    nrFrunze Empty = 0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ s1 s2 s3) = nrFrunze s1 + nrFrunze s2 + nrFrunze s3

class (Num a, Eq a) => Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

instance Scalar Int where 
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate 
  recips n = if n /= 0 then 1 `div` n else error "Cannot divide by 0"

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

-- Vector bidimensional
data Vector2D a = Vec2D a a deriving Show

-- Vector2D instanta Vector
instance Scalar a => Vector Vector2D a where
  zerov = Vec2D 0 0

  onev = Vec2D 1 1

  addv (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
  
  smult s (Vec2D x y) = Vec2D (s * x) (s * y)
  
  negatev (Vec2D x y) = Vec2D (-x) (-y)