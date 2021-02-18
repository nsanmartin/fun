import Term

data M a = Raise String | Return a deriving Show

eval :: Term -> M Int
eval (Const a) = Return a
eval (Div t u) = case eval t of
  Raise e -> Raise e
  Return a -> case eval u of
    Raise e -> Raise e
    Return b -> if b == 0 then Raise "div by 0" else Return (div a b)

-- eval :: Term -> M Int
-- eval (Const a) x = (a, x)
-- eval (Div t u) x =
--   let (a, y) = eval t x in
--     let (b, z) = eval u y in
--       (div a b, x + 1 
        


