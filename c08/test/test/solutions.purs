module Test.Cp8.Solutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref as ST

import Data.Array (head, nub, sort, tail)
import Data.Foldable (foldM)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Number (pow)

import Effect (Effect)
import Effect.Exception (error, throwException)

-- ex 1 {{{

third :: ∀ a. Array a -> Maybe a
third xs = do
  a <- tail xs
  b <- tail a
  head b

possibleSums :: Array Int -> Array Int
possibleSums = sort <<< nub <<< foldM (\sum a -> [ sum, sum + a ]) 0

filterM :: ∀ m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  p <- f x
  ps <- filterM f xs
  pure if p then x : ps else ps

{-
  map f ma = do
    a <- ma
    pure (f a)

  ap mf ma = do
    a <- ma
    f <- mf
    pure (f a)


  lift2 f (pure a) (pure b)
                                  <=> (def of lift2)
  f <$> (pure a) <*> (pure b)
                                  <=> (associativity of <*>)
  (f <$> (pure a)) <*> (pure b)
                                  <=> (expand infix)
  (map f (pure a)) <*> (pure b)
                                  <=> (def of map)
  do
    a <- pure a
    pure (f a) <*> (pure b)
                                  <=> (<*> homomorphism)
  do
    a <- pure a
    pure (f a b)
                                  <=> (monad left identity)
  pure (f a b)
-}

-- }}}

-- ex 2 }}}

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure $ a / b

estimatePi :: Int -> Number
estimatePi n | n < 1 = 0.0
estimatePi n = run do
  ref <- ST.new 0.0
  for 1 n $ toNumber >>> \k ->
    ref # ST.modify \x ->
      x + (pow (-1.0) (k + 1.0) / ((2.0 * k) - 1.0))
  (4.0 * _) <$> ST.read ref

fibonacci :: Int -> Int
fibonacci n | n < 1 = 0
fibonacci n = run do
  ref <- ST.new { x: 1, y: 0 }
  for 1 n \_ ->
    ref # ST.modify \{ x, y } -> { x: x + y, y: x }
  _.x <$> ST.read ref

-- }}}
