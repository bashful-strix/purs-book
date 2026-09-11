module Test.Cp6.Solutions where

import Prelude

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
