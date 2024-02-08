newtype Writer log a = Writer { runWriter :: (a, log) }

f :: Int -> Writer String Int
f x = if x < 0
        then Writer (-x, "negativ")
        else Writer (x, "pozitiv")

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


