{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  keys collection = map fst (toList collection)
  values collection = map snd (toList collection)
  fromList pairs = foldr(\(k, v) acc -> insert k v acc) empty pairs

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where 
    empty = PairList { getPairList = [] }

    singleton k v = PairList { getPairList = [(k, v)] }

    insert key value (PairList pairs) = 
        let existing = filter (\(existing_key, _) -> existing_key /= key) pairs 
        in PairList { getPairList = (key, value):existing }

    clookup key (PairList pairs) = 
        let elements = filter (\(k, _) -> k == key) pairs
                in 
                    if length elements == 0 then Nothing
                    else Just (snd $ head elements)

    delete key (PairList pairs) =
        let deleted = filter (\(k, _) -> k /= key) pairs
        in PairList { getPairList = deleted }

    toList (PairList pairs) = pairs
    
data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where 
    empty = Empty

    singleton k v = BNode Empty k (Just v) Empty 
    
    insert key value Empty = singleton key value
    insert key value (BNode left key' value' right)
        | key <= key' = BNode (insert key value left) key' value' right
        | otherwise = BNode left key' value' (insert key value right)

    clookup _ Empty = Nothing
    clookup searching (BNode left key value right)
        | searching == key = value
        | searching < key = clookup searching left
        | otherwise = clookup searching right
    
    delete key Empty = Empty
    delete key (BNode left key' value' right)
        | key < key' = BNode (delete key left) key' value' right
        | key > key' = BNode left key' value' (delete key right)
        | otherwise = BNode left key' Nothing right

    toList Empty = []
    toList (BNode left key Nothing right) = toList left ++ toList right
    toList (BNode left key (Just value) right) = toList left ++ [(key, value)] ++ toList right

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

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
--  (1,2,3)

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt [x]) = F x
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))

    fromArb Vid = Pt []
    fromArb (F value) = Pt []
    fromArb (N l r) = Pt ((fromArb l) ++ (fromArb r))

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where 
    perimeter (Square a) = 4 * a
    perimeter (Rectangle a b) = 2 * (a + b)
    perimeter (Circle r) = 2 * pi * r

    area (Square a) = a * a
    area (Rectangle a b) = a * b
    area (Circle r) = pi * r * r

-- egale daca au perimetrul egal
instance (Eq g, Floating g) => Eq (Geo g) where
    (==) a b = perimeter a == perimeter b