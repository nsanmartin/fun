data Term = Const Int | Div Term Term deriving Show

data M a = M a deriving Show

instance Functor M where
  fmap f m = m >>= \a -> return (f a)


instance Applicative M where
  pure = M
  (M f) <*> (M x) = M (f x)


instance Monad M where
  return = pure
  (M a) >>= k = k a


eval :: Term -> M Int
eval (Const a) = return a
eval (Div t u) = (eval t) >>=
  \a -> (eval u) >>=
  \b -> return (div a b)

