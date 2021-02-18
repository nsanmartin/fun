import Term

eval :: Term -> Int
eval (Const a) = a
eval (Div t u) = div (eval t) (eval u)

