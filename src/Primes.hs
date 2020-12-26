module Primes where

import Data.Set(fromList, toList)
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



factors :: Integer -> [Integer]
factors n
  | n < 0 = factors (-n)
  | n < 2 = []
  | otherwise = reverse $ divs n 2 []
  where divs m k ps
          | k == m = if or [elem k ps, ps == []] then k:ps else ps
          | mod m k == 0 = divs (div m k) k (k:ps)
          | otherwise = divs n (k+1) ps


is_prime :: Integer -> Bool
is_prime n = length (factors n) == 1

  
is_prime' :: Integer -> Bool
is_prime' n =
  not $ divisible n $ primes_less_than (1 + (truncate $ sqrt $ fromInteger n))
