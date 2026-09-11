module Cp4.ChapterExamples where

import Prelude hiding (gcd)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m =
  if n > m then gcd (n - m) m
  else gcd n (m - n)

gcdV2 :: Int -> Int -> Int
gcdV2 n 0 = n
gcdV2 0 m = m
gcdV2 n m
  | n > m = gcdV2 (n - m) m
  | otherwise = gcdV2 n (m - n)

-- ex 1 {{{

factorial :: Int -> Int
-- factorial n | n <= 0 = 1
--             | otherwise = n * factorial (n - 1)
factorial = factorial' 1
  where
  factorial' a n
    | n <= 0 = a
    | otherwise = factorial' (a * n) (n - 1)

binomial :: Int -> Int -> Int
binomial n k
  | k <= 0 = 1
  | k > n = 0
binomial n k = (factorial n) / ((factorial k) * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal n k
  | k <= 0 = 1
  | k > n = 0
pascal n k = (pascal n' k) + (pascal n' (k - 1))
  where
  n' = n - 1

-- }}}
