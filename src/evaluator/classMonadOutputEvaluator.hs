data Term = Const Int | Div Term Term deriving Show

data M a = M String a deriving Show

instance Functor M where
  fmap f m = m >>= \a -> return (f a)

instance Applicative M where
  pure a = M "" a
  
  (M s f) <*> (M s' x) = M (s++s') (f x)


instance Monad M where
  return = pure
  m@(M s a) >>= k = let m@(M s' b) = (k a) in M (s++s') b



out :: String -> M ()
out x = M x ()
            
line :: Term -> Int -> String
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n" 

eval :: Term -> M Int
eval k@(Const a) = out (line k a) >>= \_ -> return a
eval d@(Div t u) =
  (eval t) >>=
  \a -> (eval u) >>=
        \b ->
          (out (line d (div a b))) >>= \_ -> return (div a b)

