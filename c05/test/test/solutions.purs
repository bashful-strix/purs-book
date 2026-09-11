module Test.Cp5.Solutions where

import Prelude

import Data.Array (filter, uncons)
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

-- ex 2 {{{

squared :: Array Number -> Array Number
squared = map \x -> x * x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 6 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = ((_ >= 0.0) <$?> _)

-- }}}
