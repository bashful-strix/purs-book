module Cp2.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Cp2.Euler (answer)

main :: Effect Unit
main = do
  log $ "answer is " <> (show $ answer 1000)

