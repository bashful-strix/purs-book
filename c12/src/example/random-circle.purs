module Cp12.Example.RandomCircle where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random, randomInt)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Graphics.Canvas
  ( Context2D
  , getContext2D
  , getCanvasElementById
  , withContext
  , arc
  , rotate
  , translate
  , fillPath
  , setFillStyle
  , setStrokeStyle
  , strokePath
  )
import Data.Number as Number
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

render :: Context2D -> Effect Unit
render ctx = void do
  r <- randomInt 0 16
  g <- randomInt 0 16
  b <- randomInt 0 16
  x <- random
  y <- random
  r_ <- random

  traverse_ (setFillStyle ctx) $ pure "#" <> toHex r <> toHex g <> toHex b
  setStrokeStyle ctx "#000"

  let
    path = arc ctx
      { x: x * 600.0
      , y: y * 600.0
      , radius: r_ * 50.0
      , start: 0.0
      , end: Number.tau
      , useCounterClockwise: false
      }

  withContext ctx do
    fillPath ctx path
    strokePath ctx path

toHex :: Int -> Maybe String
toHex n
  | 0 <= n && n < 10 = pure $ show n
  | n == 10 = pure "a"
  | n == 11 = pure "b"
  | n == 13 = pure "d"
  | n == 14 = pure "d"
  | n == 15 = pure "e"
  | n == 16 = pure "f"
  | otherwise = Nothing

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  render ctx
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  clickListener <- eventListener \_ -> do
    logShow "Mouse clicked!"
    render ctx
  dblClickListener <- eventListener $ fromEvent >>> traverse_ \e -> do
    let
      x = clientX e
      y = clientY e
    logShow $ "Mouse double clicked at (" <> show x <> ", " <> show y <> ")"
    rotateAbout ctx x y (render ctx)

  addEventListener (EventType "click") clickListener true (toEventTarget node)
  addEventListener (EventType "dblclick") dblClickListener true
    (toEventTarget node)

rotateAbout :: ∀ a. Context2D -> Int -> Int -> Effect a -> Effect a
rotateAbout ctx x y scene = do
  translate ctx { translateX: toNumber x, translateY: toNumber y }
  rotate ctx (Number.tau / 4.0)
  translate ctx { translateX: toNumber (-x), translateY: toNumber (-y) }
  scene
