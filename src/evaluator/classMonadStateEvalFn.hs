data Term = Const Int | Div Term Term deriving Show
type State = Int

newtype M a = M (State -> (a, State))

unM (M f) = f




instance Functor M where
  fmap f (M fs) = M (\s -> let (a, s') = fs s in (f a, s'))

instance Applicative M where
  pure a = M (\s -> (a, s))
  (M f) <*> (M g) =
    M (\s ->
          let (a, s') = f s in
            let (b, s'') = g s' in
              (a b, s' + s''))
 
instance Monad M where
  return a = M (\s -> (a, s))
  m >>= k = M (\x -> let (a, y) = unM m x in
                       let (b, z) = unM (k a) y in
                         (b, z))


tick :: M ()
tick = M (\x -> ((), x + 1))

eval :: Term -> M Int
eval (Const a) = return a
eval (Div t u) = (eval t) >>=
  \a -> (eval u) >>=
  \b ->
    tick >>= \_ -> return (div a b)
  
