{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BlockArguments #-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
-- fct  mx =  mx  >>= (\x -> Just (pos x))
fct mx = do
    pos <$> mx

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = do
    x <- mx
    y <- my
    return (x + y)

-- cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product xs ys = do
    x <- xs
    y <- ys
    return (x,y)

-- prod f xs ys = [f x y | x <- xs, y<-ys]
prod f xs ys = do
    x <- xs
    f x <$> ys

myGetLine :: IO String
{- myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)
 -}

myGetLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- getLine
        return (x:xs)

prelNo noin =  sqrt noin

{- ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout -}

ioNumber :: IO ()
ioNumber =
    (readLn :: IO Float) >>= \noin ->
        putStrLn ("Intrare\n" ++ show noin) >>
        let noout = prelNo noin
        in (putStrLn "Iesire" >> print noout)

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name _) = "NAME: " ++ name
showPersonA :: Person -> String
showPersonA (Person _ age) = "AGE: " ++ show age

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson (Person name age) = 
    "(" ++
    showPersonN (Person name age) 
    ++ ", " 
    ++ showPersonA (Person name age) 
    ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}

newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}
mshowPersonN ::  Reader Person String
mshowPersonN = Reader (\(Person name age) -> showPersonN (Person name age))

mshowPersonA ::  Reader Person String
mshowPersonA = Reader (\(Person name age) -> showPersonA (Person name age))

mshowPerson ::  Reader Person String
mshowPerson = Reader (\(Person name age) -> showPerson (Person name age))

--- Monada Writer
newtype WriterS a = Writer { runWriter :: (a, String) } 

instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)

instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int -> WriterS Int 
logIncrement x = do 
    tell ("increment" ++ show x ++ "--")
    return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do 
    y <- logIncrement x
    logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do 
    if n == 0 then return x
    else do
        y <- logIncrement x
        logIncrementN y (n-1)