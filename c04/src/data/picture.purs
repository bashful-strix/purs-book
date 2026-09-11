module Cp4.Data.Picture where

import Prelude

import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Number (infinity, pi)
import Data.String (joinWith)

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Picture Point Number Number

{-
type Shape
  = { _tag: 'circle'; origin: Point; radius: number; }
  | { _tag: 'rectangle'; origin: Point; length: number; width: number; }
  | ...

const circleAtOrigin: Shape =
  { _tag: 'circle', origin: { x: 0, y: 0 }, radius: 10 }
-}

derive instance Eq Shape
instance Show Shape where
  show = showShape

type Point =
  { x :: Number
  , y :: Number
  }

origin :: Point
origin = { x: 0.0, y: 0.0 }

getCentre :: Shape -> Point
getCentre = case _ of
  Circle c _ -> c
  Rectangle c _ _ -> c
  Line a b -> (a + b) * { x: 0.5, y: 0.5 }
  Text l _ -> l
  Clipped _ c _ _ -> c

exampleLine :: Shape
exampleLine = Line p1 p2
  where
  p1 = { x: 0.0, y: 0.0 }
  p2 = { x: 100.0, y: 50.0 }

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [centre: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle "
    <> ("[ centre: " <> showPoint c)
    <> (", width: " <> show w)
    <> (", height: " <> show h)
    <> "]"
showShape (Line a b) =
  "Line [start: " <> showPoint a <> ", end: " <> showPoint b <> "]"
showShape (Text l t) =
  "Text [location: " <> showPoint l <> ", text: " <> show t <> "]"
showShape (Clipped p c w h) =
  "Clipped "
    <> ("[ picture: " <> (show $ showShape <$> p))
    <> (", centre: " <> showPoint c)
    <> (", width: " <> show w)
    <> (", height: " <> show h)
    <> "]"

showPoint :: Point -> String
showPoint { x, y } =
  "(" <> show x <> ", " <> show y <> ")"

-- ex 3 {{{

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCentre :: Shape -> Shape
doubleScaleAndCentre = case _ of
  Circle _ r -> Circle origin (r * 2.0)
  Rectangle _ w h -> Rectangle origin (w * 2.0) (h * 2.0)
  Text _ t -> Text origin t
  l@(Line a b) -> Line a' b'
    where
    double = { x: 2.0, y: 2.0 }
    c = getCentre l
    a' = (a - c) * double
    b' = (b - c) * double
  Clipped p _ w h ->
    Clipped (doubleScaleAndCentre <$> p) origin (w * 2.0) (h * 2.0)

shapeText :: Shape -> Maybe String
shapeText (Text _ t) = Just t
shapeText (Clipped p _ _ _) = case filterMap shapeText p of
  [] -> Nothing
  ts -> Just $ joinWith ", " ts
shapeText _ = Nothing

-- }}}

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

type Bounds =
  { top :: Number
  , left :: Number
  , bottom :: Number
  , right :: Number
  }

emptyBounds :: Bounds
emptyBounds =
  { top: -infinity
  , left: infinity
  , bottom: infinity
  , right: -infinity
  }

infiniteBounds :: Bounds
infiniteBounds =
  { top: infinity
  , left: -infinity
  , bottom: -infinity
  , right: infinity
  }

union :: Bounds -> Bounds -> Bounds
union a b =
  { top: max a.top b.top
  , left: min a.left b.left
  , bottom: min a.bottom b.bottom
  , right: max a.right b.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect a b =
  { top: min a.top b.top
  , left: max a.left b.left
  , bottom: max a.bottom b.bottom
  , right: min a.right b.right
  }

shapeBounds :: Shape -> Bounds
shapeBounds = case _ of
  Circle { x, y } r ->
    { top: y + r
    , left: x - r
    , bottom: y - r
    , right: x + r
    }
  Rectangle { x, y } w h ->
    let
      w' = w / 2.0
      h' = h / 2.0
    in
      { top: y + h'
      , left: x - w'
      , bottom: y - h'
      , right: x + w'
      }
  Line { x: ax, y: ay } { x: bx, y: by } ->
    { top: max ay by
    , left: min ax bx
    , bottom: min ay by
    , right: max ax bx
    }
  Text { x, y } _ ->
    { top: y
    , left: x
    , bottom: y
    , right: x
    }
  Clipped _ c w h ->
    shapeBounds $ Rectangle c w h

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

-- ex 5 {{{

area :: Shape -> Number
area = case _ of
  Circle _ r -> pi * r * r
  Rectangle _ w h -> w * h
  Clipped _ _ w h -> w * h
  _ -> 0.0

-- }}}
