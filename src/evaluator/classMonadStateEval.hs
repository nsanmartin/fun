data Term = Const Int | Div Term Term deriving Show
type State = Int

data M a = M State a deriving Show


instance Functor M where
  fmap f (M s a) = M s (f a)

instance Applicative M where
  pure a = M 0 a
  (M s f) <*> (M s' x) = M (s + s') (f x)


instance Monad M where
  return = pure
  m@(M s a) >>= k = let m@(M s' b) = (k a) in M (s + s') b

tick :: M ()
tick = M 1 ()

eval :: Term -> M Int
eval (Const a) = return a
eval (Div t u) = (eval t) >>=
  \a -> (eval u) >>=
  \b ->
    tick >>= \_ -> return (div a b)
