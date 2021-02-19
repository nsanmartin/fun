data Term = Const Int | Div Term Term deriving Show
type State = Int

type M a = State -> (a, State)

unit :: a -> M a
unit a x = (a, x)

(>==) :: M a -> (a -> M b) -> M b
m >== k = \x -> let (a, y) = m x in
                  let (b, z) = k a y in
                    (b, z)

tick :: M ()
tick x = ((), x + 1)

eval :: Term -> M Int
eval (Const a) = unit a
eval (Div t u) = (eval t) >==  (\a -> (eval u) >== (\b -> tick >== \_ -> unit (div a b)))
  
