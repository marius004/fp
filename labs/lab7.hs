data Expr = Const Int
    | Expr :+: Expr
    | Expr :*: Expr
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int |
    Node Operation Tree Tree
    deriving (Eq, Show)

instance Show Expr where
    show (Const x) = show x
    show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

evalExp :: Expr -> Int
evalExp (Const a) = a
evalExp (a :*: b) = evalExp a * evalExp b
evalExp (a :+: b) = evalExp a + evalExp b

evalArb :: Tree -> Int
evalArb (Lf a) = a
evalArb (Node Add a b) = evalArb a + evalArb b
evalArb (Node Mult a b) = evalArb a * evalArb b

expToArb :: Expr -> Tree
expToArb (Const a) = Lf a
expToArb (a :*: b) = Node Mult (expToArb a) (expToArb b)
expToArb (a :+: b) = Node Add (expToArb a) (expToArb b)

data IntSearchTree value
  = Empty
  | BNode
    (IntSearchTree value)     -- elemente cu cheia mai mica
    Int                       -- cheia elementului
    (Maybe value)             -- valoarea elementului
    (IntSearchTree value)     -- elemente cu cheia mai mare
    deriving (Show)

lookup' :: Ord value => value -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' searching (BNode _ _ Nothing _) = Nothing
lookup' searching (BNode left key (Just value) right)
    | searching == value = Just value
    | searching < value = lookup' searching left
    | otherwise = lookup' searching right

keys :: IntSearchTree value -> [Int]
keys Empty = []
keys (BNode _ _ Nothing _) = []
keys (BNode left key (Just value) right) = key : (keys left) ++ (keys right)

values :: IntSearchTree value -> [value]
values Empty = []
values (BNode _ _ Nothing _) = []
values (BNode left key (Just value) right) = value : (values left) ++ (values right)

insert :: Ord value => Int -> value -> IntSearchTree value -> IntSearchTree value
insert key value Empty = BNode Empty key (Just value) Empty
insert key value (BNode _ _ Nothing _) = BNode Empty key (Just value) Empty 
insert key value (BNode left_node key_node (Just value_node) right_node)
    | value < value_node = BNode (insert key value left_node) key_node (Just value_node) right_node
    | otherwise = BNode left_node key_node (Just value_node) (insert key value right_node)

tree1 =
  BNode
    (BNode Empty 5 (Just "abc") Empty)
    10
    (Just "def")
    (BNode Empty 15 (Just "ghi") Empty)

tree2 = insert 3 "foo" Empty

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete key Empty = Empty
delete key (BNode _ _ Nothing _) = Empty
delete key (BNode left_node key_node (Just value_node) right_node)
    | key == key_node = BNode left_node key_node Nothing right_node
    | otherwise = BNode (delete key left_node) key_node (Just value_node) (delete key right_node)

toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode _ _ Nothing _) = []
toList (BNode left_node key_node (Just value_node) right_node) =
    (key_node, value_node) : toList left_node ++ toList right_node

printTree :: Show value => IntSearchTree value -> String
printTree Empty = ""
printTree (BNode _ _ Nothing _) = ""
printTree (BNode left_node key_node (Just value_node) right_node) =
    "(" ++ printTree left_node ++ ") " ++ show value_node ++ " ( " ++ printTree right_node ++ ")"
