{-  
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where 
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

twoBinds :: IO()
twoBinds = 
    putStrLn "name pls: " >>
    getLine >>= 

    \name -> 
        putStrLn "age pls:" >>
        getLine >>=

    \age -> 
        putStrLn ("y helo thar: " ++ name ++ " who is : " ++ age ++ " years old.")


twoBindsMonad :: IO()
twoBindsMonad = do 
    putStrLn "name pls: "
    name <- getLine

    putStrLn "age pls: "
    age <- getLine

    putStrLn ("y helo thar: " ++ name ++ " who is : " ++ age ++ " years old.")

{- 
x1 <- e1 
x2 <- e2 
e3 
x4 <- e4
e5
-}

{- Functor si Aplicative definiti cu return si >>= 

instance Monad M where
    return a = ...
    ma >>= k = ... 

instance Applicative M where 
    pure = return
    mf <*> ma = do
        f <- ma
        a <- ma 
        return (f a)

instance Functor M where 
    fmap f ma = pure f <*> ma
-}

{-
data Maybe a = Nothing | Just a
instance Monad Maybe where 
    return = Just
    Just va >>= f = f va
    Nothing >>= _ = Nothing
-}

radical :: Float -> Maybe Float 
radical x 
    | x >= 0 = return (sqrt x)
    | x < 0 = Nothing

-- a * x^2 + b*x + c = 0
solEq2 :: Float -> Float -> Float -> Maybe Float
solEq2 0 0 0 = return 0
solEq2 0 0 c = Nothing
solEq2 0 b c = return (negate c / b)
solEq2 a b c = do 
    rDelta <- radical (b*b - 4*a*c)
    return ((negate b + rDelta) / (2 * a))

{- instance Monad [] where 
    return x = [x]
    xs >>= f = concat (map f xs)
-}

{- 
xs lista de a
f :: a -> [b]
map (f xs) = [f(x1), ..., f(xn)] = [[...], ..., [...]]
concat (map xs) = [..., ..., etc]
-}

-- >>= :: Monad [] => Monad a -> (a -> Monad b) -> Monad b

-- xs >>= f = concat (map f xs)
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do 
    x <- xs
    if even x 
        then [x*x, x*x]
        else [x*x]

solEq2All :: Float -> Float -> Float -> Maybe [Float]
solEq2All 0 0 0 = return [0]
solEq2All 0 0 c = Nothing
solEq2All 0 b c = return [(negate c / b)]
solEq2All a b c = do 
    rDelta <- radical (b*b - 4*a*c)
    return [((negate b + rDelta) / (2 * a)), ((negate b - rDelta) / (2 * a))]

-- Either monad
{- instance Monad (Either err) where
    return = Right
    Right va >>= f  = f va
    Left err >>= _ = err -}

radicalEither :: Float -> Either String Float
radicalEither x 
    | x >= 0 = return (sqrt x)
    | otherwise = Left "radical: argument negativ"

solEq2AllEither :: Float -> Float -> Float -> Either String [Float]
solEq2AllEither 0 0 0 = return [0]
solEq2AllEither 0 0 c = Left "ecuatie fara solutie"
solEq2AllEither 0 b c = return [negate c / b]
solEq2AllEither a b c = do 
    rDelta <- radicalEither (b * b - 4 * a * c)
    return [((negate b + rDelta) / (2 * a)), ((negate b - rDelta) / (2 * a))]

newtype Reader env a = Reader { runReader :: env -> a }
-- runReader :: Reader env a -> env -> a

ask :: Reader env env
ask = Reader id

instance Monad (Reader env) where
    return = Reader const
    -- return x = Reader (\_ -> x)

    ma >>= k = Reader f
        where 
            f env = let va = runReader ma env
                in runReader (k va) env

tom :: Reader String String
tom = do 
    env <- ask
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do 
    env <- ask
    return (env ++ " This is Tom.")

tomAndJerry :: Reader String String
tomAndJerry = do 
    t <- tom
    j <- jerry 
    return (t ++ "\n" ++ j)

runJerryRun :: String 
runJerryRun = runReader tomAndJerry "Who is this?"

newtype Writer log a = Writer { runWriter :: (a, log) }
-- a este parametru de tip

tell :: log -> Writer log ()
tell msg = Writer ((), msg)

instance Monad (Writer String) where
    return va = Writer(va, "")
    ma >>= f = 
        let (va, log1) = runWriter ma 
            (vb, log2) = runWriter (f va)
        in Writer (vb, log1 ++ log2)

logIncrement :: Int -> Writer String Int 
logIncrement x = do 
    tell ("increment" ++ show x ++ "--")
    return (x + 1)

logIncrement2 :: Int -> Writer String Int
logIncrement2 x = do 
    y <- logIncrement x
    logIncrement y

-- runWriter (logIncrement2 13)
-- (15, "increment:13--increment14")

