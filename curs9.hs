import Control.Monad.Trans.Writer (Writer)

f :: Int -> Writer Int String
f x = if x < 0 then Writer (-x, "negativ")
        else Writer (x, "pozitiv")