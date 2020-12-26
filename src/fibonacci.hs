fibonacci :: Integer -> Integer
fibonacci n = fib 0 1 n
  where fib n2 n1 0 = n2 + n1
        fib n2 n1 k = let n0 = n2 + n1 in fib n1 n0 (k-1)
