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

-- ex 2 {{{

-- sameCity :: Person -> Person -> Boolean
sameCity
  :: ∀ c pr1 pr2 ar1 ar2
   . Eq c
  => { address :: { city :: c | ar1 } | pr1 }
  -> { address :: { city :: c | ar2 } | pr2 }
  -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: ∀ a. a -> Array a -> a
fromSingleton _ [ a ] = a
fromSingleton a0 _ = a0

-- }}}

-- ex 4 {{{

newtype Volt = Volt Number
newtype Ohm = Ohm Number
newtype Amp = Amp Number
newtype Watt = Watt Number

calculateCurrent :: Volt -> Ohm -> Amp
calculateCurrent (Volt v) (Ohm 0.0) = Amp v
calculateCurrent (Volt v) (Ohm r) = Amp (v / r)

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

battery :: Volt
battery = Volt 1.5

lightbulb :: Ohm
lightbulb = Ohm 500.0

current :: Amp
current = calculateCurrent battery lightbulb

-- }}}
