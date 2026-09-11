module Cp12.Example.LSystem where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Writer.Trans (WriterT, tell, execWriterT)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Array (concatMap, foldM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Graphics.Canvas
  ( Context2D
  , closePath
  , fillPath
  , lineTo
  , getContext2D
  , getCanvasElementById
  , setShadowBlur
  , setShadowColor
  , setShadowOffsetX
  , setShadowOffsetY
  )
import Data.Number as Number
import Partial.Unsafe (unsafePartial)

lsystem
  :: forall a m s
   . Monad m
  => Array a
  -> (a -> Array a)
  -> (s -> a -> m s)
  -> Int
  -> s
  -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s i = go (concatMap prod s) (i - 1)

expand :: ∀ a. (a -> Array a) -> Int -> Array a -> Array a
expand _ 0 s = s
expand prod n s = expand prod (n - 1) (concatMap prod s)

interpFold :: ∀ a m s. Monad m => (s -> a -> m s) -> s -> Array a -> m s
interpFold = foldM

type Angle = Number

-- data Letter = L Angle | R Angle | F

data Letter = L Angle | R Angle | F Boolean

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

-- initial :: Sentence
-- initial =
--   [ F
--   , R (Number.tau / 6.0)
--   , R (Number.tau / 6.0)
--   , F
--   , R (Number.tau / 6.0)
--   , R (Number.tau / 6.0)
--   , F
--   , R (Number.tau / 6.0)
--   , R (Number.tau / 6.0)
--   ]

-- productions :: Letter -> Sentence
-- productions (L a) = [ L a ]
-- productions (R a) = [ R a ]
-- productions F =
--   [ F
--   , L (Number.tau / 6.0)
--   , F
--   , R (Number.tau / 6.0)
--   , R (Number.tau / 6.0)
--   , F
--   , L (Number.tau / 6.0)
--   , F
--   ]

initial :: Sentence
initial = [ F true ]

productions :: Letter -> Sentence
productions (L a) = [ L a ]
productions (R a) = [ R a ]
productions (F x) =
  [ F x
  , (if x then R else L) (Number.tau / 6.0)
  , F (not x)
  , (if x then R else L) (Number.tau / 6.0)
  , F x
  , (if x then L else R) (Number.tau / 6.0)
  , F (not x)
  , (if x then L else R) (Number.tau / 6.0)
  , F x
  , (if x then L else R) (Number.tau / 6.0)
  , F (not x)
  , (if x then L else R) (Number.tau / 6.0)
  , F x
  , (if x then R else L) (Number.tau / 6.0)
  , F (not x)
  , (if x then R else L) (Number.tau / 6.0)
  , F x
  ]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

interpretR
  :: State
  -> Letter
  -> ReaderT Context2D (WriterT (Additive Int) Effect) State
interpretR state (L a) = pure $ state { theta = state.theta - a }
interpretR state (R a) = pure $ state { theta = state.theta + a }
interpretR state _ = do
  let
    x = state.x + Number.cos state.theta * 1.5
    y = state.y + Number.sin state.theta * 1.5
  tell (Additive 1)
  ctx <- ask
  liftEffect do
    lineTo ctx x y
    pure { x, y, theta: state.theta }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setShadowBlur ctx 2.0
  setShadowColor ctx "#f00"
  setShadowOffsetX ctx 5.0
  setShadowOffsetY ctx 2.5

  steps <- fillPath ctx $ execWriterT $ flip runReaderT ctx
    -- $ lsystem initial productions interpretR 5 initialState
    $ foldM interpretR initialState
    $ expand productions 3 initial
  closePath ctx

  Console.logShow steps
