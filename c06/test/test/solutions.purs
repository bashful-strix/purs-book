module Test.Cp6.Solutions where

import Prelude

import Data.Array (nub, nubEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

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
