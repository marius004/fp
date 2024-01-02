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

class Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

instance Scalar Int where
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x /= 0 then 1 else error "Cannot reciprocate zero"

instance Scalar Integer where
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x /= 0 then 1 else error "Cannot reciprocate zero"

-- Vector bidimensional
data Vector2D a = Vec2D a a deriving Show

instance Scalar a => Vector Vector2D a where
  zerov = Vec2D zero zero
  addv (Vec2D x1 y1) (Vec2D x2 y2) = Vec2D (adds x1 x2) (adds y1 y2)
  smult :: Scalar a => a -> Vector2D a -> Vector2D a
  smult s (Vec2D x y) = Vec2D (mult s x) (mult s y)
  negatev (Vec2D x y) = Vec2D (negates x) (negates y)

-- Vector tridimensional
data Vector3D a = Vec3D a a a deriving Show

instance Scalar a => Vector Vector3D a where
  zerov = Vec3D zero zero zero
  addv :: Scalar a => Vector3D a -> Vector3D a -> Vector3D a
  addv (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (adds x1 x2) (adds y1 y2) (adds z1 z2)
  smult s (Vec3D x y z) = Vec3D (mult s x) (mult s y) (mult s z)
  negatev (Vec3D x y z) = Vec3D (negates x) (negates y) (negates z)