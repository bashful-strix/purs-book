module Test.Cp11.Solutions where

import Prelude
import PointFree ((~$))

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Trans (ask, local)
import Control.Monad.State (execState, modify)

import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse_)

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

-- ex 2 {{{

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line l = do
  i <- ask
  pure $ (power "  " i) <> l

indent :: Doc -> Doc
indent =
  local (_ + 1)

cat :: Array Doc -> Doc
cat =
  map (joinWith "\n") <<< sequence

render :: Doc -> String
render =
  runReader ~$ 0

-- }}}
