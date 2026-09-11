module Test.Cp8.Solutions where

import Prelude

import Data.Array (head, nub, sort, tail)
import Data.Foldable (foldM)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)

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
