module Cp12.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Cp12.Example.Rectangle (main) as Rectange

main :: Effect Unit
main = do
  log "🍝"

  Rectange.main
