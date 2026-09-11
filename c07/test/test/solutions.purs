module Test.Cp7.Solutions where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V)

import Cp7.Data.AddressBook (Address, address)
import Cp7.Data.AddressBook.Validation (Errors, matches)

-- ex 1 {{{

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe = lift2 (+)

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe = lift2 (-)

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe = lift2 (*)

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe = lift2 (/)

addApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 (+)

subApply :: ∀ f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 (-)

mulApply :: ∀ f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 (*)

divApply :: ∀ f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 (/)

combineMaybe :: ∀ f a. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = Just <$> fa

-- }}}

-- ex2 {{{

stateRegex :: Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "[^\\s]+" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex a.street
  city <- matches "City" nonEmptyRegex a.city
  state <- matches "State" stateRegex a.state
  in address street city state

-- }}}
