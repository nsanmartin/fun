data Term = Const Int | Div Term Term deriving Show

type M a = (String, a)

unit :: a -> M a
unit a = ("", a)

(>==) :: M a -> (a -> M b) -> M b
m >== k = let (x, a) = m in
            let (y, b) = k a in
              (x ++ y, b)

out :: String -> M ()
out x = (x, ())
            
line :: Term -> Int -> String
line t a = "eval (" ++ show t ++ ") <= " ++ show a ++ "\n" 


eval :: Term -> M Int
eval k@(Const a) = out (line k a) >== \_ -> unit a
eval d@(Div t u) =
  (eval t) >==
  \a -> (eval u) >==
        \b ->
          (out (line d (div a b))) >== \_ -> unit (div a b)
  
