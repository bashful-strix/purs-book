module Cp11.Main where

import Prelude

import Control.Monad.RWS (RWSResult(..), runRWST)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Newtype (wrap)
import Data.String (split)

import Effect (Effect)
import Effect.Console (log)

import Node.ReadLine as RL

import Options.Applicative ((<**>))
import Options.Applicative as OP

import Cp11.Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Cp11.Data.GameState (GameState, initialGameState)

import Cp11.Game (game)

runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      next <-
        case
          runExcept $ runRWST (game (split (wrap " ") input)) env currentState
          of
          Left errs -> do
            for_ errs (log <<< ("Err: " <> _))
            pure currentState
          Right (RWSResult state _ written) -> do
            for_ written log
            pure state

      RL.prompt interface
      RL.question "" (lineHandler next) interface

  RL.prompt interface
  RL.question "" (lineHandler initialGameState) interface

main :: Effect Unit
main = OP.customExecParser prefs argParser >>= runGame
  where

  argParser :: OP.ParserInfo GameEnvironment
  argParser = OP.info (env <**> OP.helper) parserOptions

  env :: OP.Parser GameEnvironment
  env = gameEnvironment <$> player <*> debug <*> cheat

  player :: OP.Parser String
  player = OP.strOption $ fold
    [ OP.long "player"
    , OP.short 'p'
    , OP.metavar "<player name>"
    , OP.help "The player's name <String>"
    ]

  debug :: OP.Parser Boolean
  debug = OP.switch $ fold
    [ OP.long "debug"
    , OP.short 'd'
    , OP.help "Use debug mode"
    ]

  cheat :: OP.Parser Boolean
  cheat = OP.switch $ fold
    [ OP.long "cheat"
    , OP.short 'c'
    , OP.help "Use cheat mode"
    ]

  prefs = OP.prefs OP.showHelpOnEmpty

  parserOptions = fold
    [ OP.fullDesc
    , OP.progDesc "Play the game as <player name>"
    , OP.header "Monadic Adventures! A game to learn monad transformers"
    ]
