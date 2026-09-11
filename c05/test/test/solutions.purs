module Test.Cp5.Solutions where

import Prelude

import Control.Alternative (guard)
import Data.Array ((:), (..), filter, length, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

import Cp5.ChapterExamples (factors)

-- ex 1 {{{

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not $ isEven $ if n > 0 then n - 1 else n + 1

countEven :: Array Int -> Int
countEven xs = case uncons xs of
  Nothing -> 0
  Just { head, tail } -> countEven tail + if isEven head then 1 else 0

-- }}}

-- ex 2 {{{

squared :: Array Number -> Array Number
squared = map \x -> x * x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 6 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = ((_ >= 0.0) <$?> _)

-- }}}

-- ex 3 {{{

isPrime :: Int -> Boolean
isPrime n | n <= 1 = false
isPrime n =
  eq 1 $ length $ factors n

cartesianProduct :: ∀ a. Array a -> Array a -> Array (a /\ a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure $ a /\ b

triples :: Int -> Array (Int /\ Int /\ Int)
triples n = do
  a <- 1 .. n
  b <- a .. n -- after a to prevent dupes
  c <- b .. n -- c must be at least as big as b due to (+ a^2)
  guard $ (a * a) + (b * b) == c * c
  pure $ a /\ b /\ c

primeFactors :: Int -> Array Int
primeFactors = factorise 2
  where
  factorise _ 1 = []
  factorise x n
    | n `mod` x == 0 = x : factorise x (n / x)
    | otherwise = factorise (x + 1) n

-- }}}
