module Test.Cp5.Solutions where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..))

-- ex 1 {{{

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not $ isEven $ if n > 0 then n - 1 else n + 1

countEven :: Array Int -> Int
countEven xs = case uncons xs of
  Nothing -> 0
  Just { head, tail } -> countEven tail + if isEven head then 1 else 0

-- }}}
