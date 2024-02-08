import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  deriving Eq
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not(Var "Q"))

instance Show Prop where
  show F = show F
  show T = show T
  show (Var name) = name
  show (Not p) = "(~" ++ show p ++ ")"
  show (p :&: q) = "(" ++ show p ++ "&" ++ show q ++ ")"
  show (p :|: q) = "(" ++ show p ++ "|" ++ show q ++ ")"

test_ShowProp = Not (Var "P") :&: Var "Q"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var var) env = impureLookup var env
eval F _ = False
eval T _ = True
eval (Not prop) env = not (eval prop env)
eval (p1 :|: p2) env = eval p1 env || eval p2 env
eval (p1 :&: p2) env = eval p1 env && eval p2 env

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)]

variabile :: Prop -> [Nume]
variabile (Var name) = [name]
variabile F = []
variabile T = []
variabile (Not prop) = variabile prop
variabile (p1 :&: p2) = nub $ variabile p1 ++ variabile p2
variabile (p1 :|: p2) = nub $ variabile p1 ++ variabile p2

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (var:vars) = [(var, val):env | env <- envs vars, val <- [False, True]]

test_envs = envs ["P", "Q"]

satisfiabila :: Prop -> Bool
satisfiabila prop = or [eval prop combination | combination <- combinations]
    where combinations = envs $ variabile prop

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q")
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P")

valida :: Prop -> Bool
valida prop = and [eval prop combination | combination <- combinations]
    where combinations = envs $ variabile prop

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2
    | length var_p1 /= length var_p2 = False
    | otherwise = all (\(x, y) -> x == y) [(eval p1 state1, eval p2 state2) | (state1, state2) <- zip var_p1 var_p2]
  where
    var_p1 = envs $ variabile p1
    var_p2 = envs $ variabile p2

test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))