module Cp12.Example.Rectangle where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, tau) as Number
import Data.Traversable (traverse_)

import Effect (Effect)

import Graphics.Canvas
  ( Context2D
  , arc
  , rect
  , lineTo
  , moveTo
  , fillPath
  , setFillStyle
  , getContext2D
  , getCanvasElementById
  )
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  fillPath ctx $ do
    rect ctx
      { x: 190.0
      , y: 250.0
      , width: 100.0
      , height: 100.0
      }

    rect ctx
      { x: 310.0
      , y: 250.0
      , width: 100.0
      , height: 100.0
      }

    moveTo ctx 300.0 200.0
    arc ctx
      { x: 300.0
      , y: 200.0
      , radius: 50.0
      , start: 0.0
      , end: Number.tau * 1.0 / 2.0
      , useCounterClockwise: false
      }
    lineTo ctx 300.0 100.0
    lineTo ctx 350.0 200.0

  moveTo ctx 300.0 450.0
  samplePath ctx 10 \n ->
    { x: 300.0 + (50.0 * Number.sin (n * Number.tau))
    , y: 450.0 + (50.0 * Number.cos (n * Number.tau))
    }

type Point = { x :: Number, y :: Number }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx =
  fillPath ctx <<< traverse_ \{ x, y } -> lineTo ctx x y

samplePath :: Context2D -> Int -> (Number -> Point) -> Effect Unit
samplePath ctx n f =
  renderPath ctx ((f <<< (_ / toNumber n) <<< toNumber) <$> 0 .. n)
