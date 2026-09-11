module Test.Cp10.Solutions where

import Data.Function.Uncurried (Fn3)

-- ex 1 {{{

foreign import volumeFn :: Fn3 Number Number Number Number
foreign import volumeArrow :: Number -> Number -> Number -> Number

-- }}}
