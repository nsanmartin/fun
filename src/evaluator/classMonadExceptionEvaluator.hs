data Term = Const Int | Div Term Term deriving Show

data M a = Raise String | Return a deriving Show

instance Functor M where
  fmap f (Raise s) = (Raise s)
  fmap f (Return a) = Return (f a)


instance Applicative M where
  pure = Return
  
  Return f <*> Return x = Return (f x)
  Raise e <*> _ = Raise e
  _ <*> Raise e = Raise e


instance Monad M where
  return = pure
  m >>= k = case m of
    Raise e -> Raise e
    Return a -> k a


raise :: String -> M a
raise e = Raise e


eval :: Term -> M Int
eval (Const a) = return a
eval (Div t u) = (eval t) >>=
  \a -> (eval u) >>=
  \b ->
    if b == 0 then raise "div by 0" else return $ div a b

