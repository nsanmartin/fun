suma :: [Int] -> Int
suma ns = suma' ns 0
  where suma' [] s = s
        suma' (x:xs) s = let s' = s + x in seq s' (suma' xs s')

