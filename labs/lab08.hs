-- clasa Collection 
{-# LANGUAGE InstanceSigs #-}
import Prelude hiding (lookup)
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key, value)] -> c key value

  keys collection = map fst (toList collection)

  values collection = map snd (toList collection)

  fromList pairs = foldr (\(k, v) acc -> insert k v acc) empty pairs

newtype Map key value = Map [(key, value)]

instance Collection Map where
  empty = Map []
  
  singleton k v = Map [(k, v)]
  
  insert k v (Map pairs) = Map $ (k, v) : filter (\(k', _) -> k' /= k) pairs
  
  lookup k (Map pairs) = lookupByKey k pairs
    where
      lookupByKey _ [] = Nothing
      lookupByKey key ((k', v'):rest)
        | key == k' = Just v'
        | otherwise = lookupByKey key rest
  
  delete k (Map pairs) = Map $ filter (\(k', _) -> k' /= k) pairs
  
  keys (Map pairs) = map fst pairs
  
  values (Map pairs) = map snd pairs
  
  toList (Map pairs) = pairs
  
  fromList pairs = Map pairs

newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty = PairList []
  
  singleton k v = PairList [(k, v)]
  
  insert k v (PairList pairs) = PairList $ (k, v) : filter (\(k', _) -> k' /= k) pairs
  
  lookup k (PairList pairs) = lookupByKey k pairs
    where
      lookupByKey _ [] = Nothing
      lookupByKey key ((k', v'):rest)
        | key == k' = Just v'
        | otherwise = lookupByKey key rest
  
  delete k (PairList pairs) = PairList $ filter (\(k', _) -> k' /= k) pairs
  
  keys (PairList pairs) = map fst pairs
  
  values (PairList pairs) = map snd pairs
  
  toList (PairList pairs) = pairs
  
  fromList pairs = PairList pairs

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
      deriving Show

instance Collection SearchTree where
  empty = Empty
  
  singleton key value = BNode Empty key (Just value) Empty

  insert key value Empty = singleton key value
  insert key value (BNode left key' value' right)
    | key < key' = BNode (insert key value left) key' value' right
    | key > key' = BNode left key' value' (insert key value right)
    | otherwise = BNode left key (Just value) right

  lookup _ Empty = Nothing
  lookup searching (BNode left key value right)
    | searching == key = value
    | searching < key = lookup searching left
    | otherwise = lookup searching right
  
  delete key Empty = Empty
  delete key (BNode left key' value' right)
    | key < key' = BNode (delete key left) key' value' right
    | key > key' = BNode left key' value' (delete key right)
    | otherwise = BNode left key' Nothing right

  keys Empty = []
  keys (BNode left key _ right) = keys left ++ [key] ++ keys right

  values Empty = []
  values (BNode left _ Nothing right) = values left ++ values right
  values (BNode left _ (Just value) right) = values left ++ [value] ++ values right

  toList Empty = []
  toList (BNode left key Nothing right) = toList left ++ toList right
  toList (BNode left key (Just value) right) = toList left ++ [(key, value)] ++ toList right

  fromList pairs = foldr (\(k, v) acc -> insert k v acc) empty pairs

-- Puncte puncte
data Arb = Vid | F Int | N Arb Arb deriving Show
data Punct = Pt [Int]

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance Show Punct where 
    show (Pt ys)  = "(" ++ showPct ys ++ ")"
        where 
            showPct [] = ""
            showPct [x] = show x
            showPct (x:xs) = show x ++ "," ++ showPct xs

-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
-- (1,2,3)

-- data Arb = Vid | F Int | N Arb Arb deriving Show

instance ToFromArb Punct where 
    toArb :: Punct -> Arb
    toArb (Pt []) = Vid 
    toArb (Pt [x]) = N (F x) Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb :: Arb -> Punct
    fromArb Vid = Pt []
    fromArb (F value) = Pt [value]
    fromArb (N l r) = Pt $ fromArb l ++ fromArb r -- does not work for a reason

-- figuri geometrice
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
  perimeter (Square side) = 4 * side
  perimeter (Rectangle length width) = 2 * (length + width)
  perimeter (Circle radius) = 2 * pi * radius

  area (Square side) = side * side
  area (Rectangle length width) = length * width
  area (Circle radius) = pi * radius * radius

instance (Floating a, Eq a) => Eq (Geo a) where
  geo1 == geo2 = perimeter geo1 == perimeter geo2