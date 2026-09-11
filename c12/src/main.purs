module Cp12.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Cp12.Example.LSystem (main) as LSystem
-- import Cp12.Example.Random (main) as Random
-- import Cp12.Example.RandomCircle (main) as RandomCircle
-- import Cp12.Example.Refs (main) as Refs
-- import Cp12.Example.Rectangle (main) as Rectange
-- import Cp12.Example.Shapes (main) as Shapes

main :: Effect Unit
main = do
  log "🍝"

  -- Rectange.main
  -- Shapes.main
  -- Random.main
  -- Refs.main
  LSystem.main

  -- ex
  -- RandomCircle.main
