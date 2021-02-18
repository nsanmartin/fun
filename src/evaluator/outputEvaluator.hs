import Term

type M a = (String, a)

eval :: Term -> M Int
eval k@(Const a) = (line k a, a)
eval d@(Div t u) = let (x, a) = eval t in
                     let (y, b) = eval u in
                       (x ++ y ++ line d (div a b), (div a b))
                       


line :: Term -> Int -> String
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n" 
