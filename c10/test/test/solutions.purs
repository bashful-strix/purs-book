module Test.Cp10.Solutions where

import Data.Function.Uncurried (Fn3)
import Test.Cp10.Examples (Complex)

-- ex 1 {{{

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

-- }}}

-- ex 2 {{{

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

-- }}}
