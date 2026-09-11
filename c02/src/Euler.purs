module Cp2.Euler where

import Prelude

import Data.Foldable (sum)
import Data.List (List, range, filter)
import Data.Int (rem)
import Data.Number (pi, sqrt)

ns :: Int -> List Int
ns n = range 0 (n - 1)

multiples :: Int -> List Int
multiples n = filter (\x -> mod x 3 == 0 || mod x 5 == 0) (ns n)

answer :: Int -> Int
answer n = sum (multiples n)

-- ex 1 {{{

diagonal :: Number -> Number -> Number
diagonal a b = sqrt (a * a + b * b)

-- }}}

-- ex 2 {{{

circleArea :: Number -> Number
circleArea r = pi * r * r

leftoverCents :: Int -> Int
leftoverCents n = rem n 100

-- }}}
