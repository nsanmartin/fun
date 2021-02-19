data Term = Const Int | Div Term Term deriving Show

data M a = Raise String | Return a deriving Show

unit :: a -> M a
unit a = Return a

(>==) :: M a -> (a -> M b) -> M b
m >== k = case m of
  Raise e -> Raise e
  Return a -> k a


raise :: String -> M a
raise e = Raise e


eval :: Term -> M Int
eval (Const a) = unit a
eval (Div t u) = (eval t) >==
  \a -> (eval u) >==
  \b ->
    if b == 0 then raise "div by 0" else unit $ div a b
  
