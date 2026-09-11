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
