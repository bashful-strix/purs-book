module Test.Cp6.Solutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

import Cp6.Data.Hashable (class Hashable, hash, hashEqual)

-- ex 1 {{{

newtype Point = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

derive newtype instance Eq Point
derive newtype instance Ord Point

-- }}}

-- ex2 {{{

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance Show Complex where
  show (Complex { real, imaginary }) =
    show real <> sign <> show imaginary <> "i"
    where
    sign = if imaginary >= 0.0 then "+" else ""

derive instance Eq Complex

derive instance Newtype Complex _

instance Semiring Complex where
  add = over2 Complex add
  zero = wrap zero
  mul
    (Complex { real: a, imaginary: b })
    (Complex { real: c, imaginary: d }) = Complex
    { real: (a * c) - (b * d)
    , imaginary: (a * d) + (b * c)
    }
  one = Complex { real: 1.0, imaginary: 0.0 }

derive newtype instance Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance Generic Shape _

instance Show Shape where
  show = genericShow

derive instance Eq Shape
instance Ord Shape where
  compare = genericCompare

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

-- }}}

-- ex 3 {{{

data NonEmpty a = NonEmpty a (Array a)

derive instance Generic (NonEmpty a) _
instance Show a => Show (NonEmpty a) where
  show = genericShow

-- derive instance Eq a => Eq (NonEmpty a)
instance Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a as) (NonEmpty b bs) = a == b && as == bs

instance Semigroup (NonEmpty a) where
  append (NonEmpty a as) (NonEmpty b bs) = NonEmpty a (as <> [ b ] <> bs)

instance Functor NonEmpty where
  map f (NonEmpty a as) = NonEmpty (f a) (f <$> as)

instance Foldable NonEmpty where
  -- foldr f b (NonEmpty a as) = foldr f b ([ a ] <> as)
  -- foldl f b (NonEmpty a as) = foldl f b ([ a ] <> as)
  foldr f b (NonEmpty a as) = f a (foldr f b as)
  foldl f b (NonEmpty a as) = foldl f (f b a) as
  foldMap f (NonEmpty a as) = f a <> foldMap f as

data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite a) (Finite b) = compare a b

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldr f b (OneMore a as) = f a (foldr f b as)
  foldl f b (OneMore a as) = foldl f (f b a) as
  foldMap f (OneMore a as) = f a <> foldMap f as

-- }}}

-- ex 4 {{{

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum = fromJust <<< maximum

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance Show Multiply
derive newtype instance Eq Multiply

instance Semigroup Multiply where
  append (Multiply a) (Multiply b) = Multiply (a * b)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply a) x = a * x

-- valid but not accepted
-- act _ x = x

-- act (Multiply a) x = x / a

-- act (Multiply a) x = pow x a

-- act (Multiply a) 1 = a
-- act a x = act (a <> Multiply x) 1

instance Action Multiply String where
  act (Multiply a) x = power x a

instance Action m a => Action m (Array a) where
  act a xs = act a <$> xs

newtype Self m = Self m

derive newtype instance Show m => Show (Self m)
derive newtype instance Eq m => Eq (Self m)

instance Monoid m => Action m (Self m) where
  act a (Self b) = Self (a <> b)

-- }}}

-- ex 5 {{{

arrayHasDuplicates :: ∀ a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs =
  length xs /= length (nubByEq (hashEqual && eq) xs)

newtype Hour = Hour Int

instance Eq Hour where
  eq (Hour a) (Hour b) = mod a 12 == mod b 12

instance Hashable Hour where
  hash (Hour a) = hash $ mod a 12

-- }}}
