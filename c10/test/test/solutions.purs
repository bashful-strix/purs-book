module Test.Cp10.Solutions where

import Prelude

import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))

import Test.Cp10.Examples (Complex, Undefined)

-- ex 1 {{{

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

-- }}}

-- ex 2 {{{

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

-- }}}

-- ex 3 {{{

type Quadratic =
  { a :: Number
  , b :: Number
  , c :: Number
  }

foreign import quadraticRootsImpl
  :: (∀ a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots = quadraticRootsImpl Pair

foreign import toMaybeIml
  :: ∀ a. (∀ x. x -> Maybe x) -> (∀ x. Maybe x) -> Undefined a -> Maybe a

toMaybe :: ∀ a. Undefined a -> Maybe a
toMaybe = toMaybeIml Just Nothing

-- }}}
