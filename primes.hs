divisible :: Integer -> [Integer] -> Bool
divisible n ps = or [ mod n k == 0 | k <- filter ((<= n) . (^2)) ps ]

primes :: Integer -> [Integer]
primes 0 = []
primes n = find_primes n [2]
  where find_primes 1 ps = ps
        find_primes k ps =
          let m = 1 + head ps in seq ps find_primes (k-1) $ push_next m ps
        push_next n ps
          | divisible n ps = push_next (n+1) ps
          | otherwise = n:ps
        
          
primes_less_than :: Integer -> [Integer]
primes_less_than n
  | n <= 2 = []
  | otherwise = push_next 2 []
  where push_next k ps
          | n == k = ps
          | divisible k ps = push_next (k+1) ps
          | otherwise = let ps' = k:ps in seq ps' push_next (k+1) (k:ps)

is_prime :: Integer -> Bool
is_prime n =
  not $ divisible n $ primes_less_than (1 + (truncate $ sqrt $ fromInteger n))

first_divisor :: Integer -> Maybe Integer
first_divisor n
  | ps == [] = Nothing
  | otherwise = Just $ head ps
  where ps = divs n $ primes_less_than (1 + (truncate $ sqrt $ fromInteger n))
        divs m ps = filter ((==0) . (mod m)) ps
