data Term = Const Int | Div Term Term deriving Show

type M a = a
unit :: a -> M a
unit a = a

(>==) :: M a -> (a -> M b) -> M b
a >== k = k a


eval :: Term -> M Int
eval (Const a) = unit a
eval (Div t u) = (eval t) >==  (\a -> (eval u) >== (\b -> unit $ div a b))
  
