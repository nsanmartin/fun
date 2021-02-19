import Term

type M a = State -> (a, State)
type State = Int


eval :: Term -> M Int
eval (Const a) s = (a, s)
eval (Div t u) s = let (a, s') = eval t s in
                     let (b, s'') = eval u s' in
                       (div a b, s'' + 1)
        


