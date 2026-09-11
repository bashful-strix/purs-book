module Test.Cp11.Solutions where

import Prelude
import PointFree ((~$))

import Control.Monad.State (execState, modify)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse_)

-- ex 1 {{{

testParens :: String -> Boolean
testParens =
  toCharArray
    >>> traverse_
      ( \c -> modify \t -> t +
          if c == '(' && t >= 0 then 1
          else if c == ')' then -1
          else 0
      )
    >>> (execState ~$ 0)
    >>> eq 0

-- }}}
