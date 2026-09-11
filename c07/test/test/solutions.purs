module Test.Cp7.Solutions where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

import Cp7.Data.AddressBook (Address, PhoneNumber, address)
import Cp7.Data.AddressBook.Validation
  ( Errors
  , matches
  , nonEmpty
  , validateAddress
  , validatePhoneNumbers
  )

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

-- ex 3 {{{

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

derive instance Eq a => Eq (Tree a)
derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show a = genericShow a

instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l a r) = Branch (map f l) (f a) (map f r)

instance Foldable Tree where
  foldr f b = case _ of
    Leaf -> b
    Branch l a r ->  foldr f (f a (foldr f b r)) l
  foldl f b = case _ of
    Leaf -> b
    Branch l a r -> foldl f (f (foldl f b l) a) r
  foldMap f = case _ of
    Leaf -> mempty
    Branch l a r -> foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse f = case _ of
    Leaf -> pure Leaf
    Branch l a r -> Branch <$> traverse f l <*> f a <*> traverse f r
  -- sequence = traverse identity
  sequence = case _ of
    Leaf -> pure Leaf
    Branch l a r -> Branch <$> sequence l <*> a <*> sequence r

traversePreOrder
  :: ∀ m a b
   . Applicative m
  => (a -> m b)
  -> Tree a
  -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch l a r) = ado
  a' <- f a
  l' <- traversePreOrder f l
  r' <- traversePreOrder f r
  in Branch l' a' r'

traversePostOrder
  :: ∀ m a b
   . Applicative m
  => (a -> m b)
  -> Tree a
  -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch l a r) = ado
  l' <- traversePostOrder f l
  r' <- traversePostOrder f r
  a' <- f a
  in Branch l' a' r'

type PersonOptionalAddress =
  { firstName :: String
  , lastName :: String
  , homeAddress :: Maybe Address
  , phones :: Array PhoneNumber
  }

validatePersonOptionalAddress
  :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p = ado
  firstName   <- nonEmpty "First Name" p.firstName
  lastName    <- nonEmpty "Last Name" p.lastName
  homeAddress <- traverse validateAddress p.homeAddress
  phones      <- validatePhoneNumbers "Phone Numbers" p.phones
  in { firstName, lastName, homeAddress, phones }

traverseUsingSequence
  :: ∀ f m a b
   . Traversable f
  => Applicative m
  => (a -> m b)
  -> f a
  -> m (f b)
traverseUsingSequence fn = sequence <<< map fn

sequenceUsingTraverse
  :: ∀ f m a
   . Traversable f
  => Applicative m
  => f (m a)
  -> m (f a)
sequenceUsingTraverse = traverse identity

-- }}}
