module Cp12.Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Number as Number

import Effect (Effect)
import Effect.Random (random)

import Graphics.Canvas
  ( Context2D
  , strokePath
  , fillPath
  , arc
  , setStrokeStyle
  , setFillStyle
  , getContext2D
  , getCanvasElementById
  )

import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let
      path = arc ctx
        { x: x * 600.0
        , y: y * 600.0
        , radius: r * 50.0
        , start: 0.0
        , end: Number.tau
        , useCounterClockwise: false
        }

    strokeFill ctx path

strokeFill :: ∀ a. Context2D -> Effect a -> Effect a
strokeFill ctx path =
  fillPath ctx path *>
  strokePath ctx path
