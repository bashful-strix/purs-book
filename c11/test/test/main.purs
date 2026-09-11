module Test.Cp11.Main where

import Prelude

import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.RWS.Trans (RWSResult(..), runRWST)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT, execWriter)

import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Set as S
import Data.Tuple (Tuple(..))

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp11.Game (Game, cheat, move, pickUp)
import Cp11.Data.GameEnvironment (GameEnvironment(..))
import Cp11.Data.GameItem (GameItem(..))
import Cp11.Data.GameState (GameState(..), initialGameState)
import Test.Cp11.Solutions
  ( testParens

  , line
  , indent
  , cat
  , render

  , sumArrayWriter
  , collatz

  , safeDivide
  , string
  , line'
  , indent'
  , render'

  , asFollowedByBs
  , asOrBs
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Exercises Group - The State Monad" do
    describe "testParens" do
      let
        runTestParens expected str =
          it testName do
            testParens str `shouldEqual` expected
          where
          testName = "str = \"" <> str <> "\""

      runTestParens true ""
      runTestParens true "(()(())())"
      runTestParens true "(hello)"
      runTestParens false ")"
      runTestParens false "(()()"
      runTestParens false ")("

  describe "Exercises Group - The Reader Monad" do
    describe "indents" do
      let
        expectedText =
          "Here is some indented text:\n\
          \  I am indented\n\
          \  So am I\n\
          \    I am even more indented"

      it "should render with indentations" do
        ( render $ cat
            [ line "Here is some indented text:"
            , indent $ cat
                [ line "I am indented"
                , line "So am I"
                , indent $ line "I am even more indented"
                ]
            ]
        ) `shouldEqual` expectedText

  describe "Exercises Group - The Writer Monad" do
    describe "sumArrayWriter" do
      it "should sum arrays" do
        ( execWriter $ do
            sumArrayWriter [ 1, 2, 3 ]
            sumArrayWriter [ 4, 5 ]
            sumArrayWriter [ 6 ]
        ) `shouldEqual` (Additive 21)

    describe "collatz" do
      let
        expected_11 =
          Tuple 14 [ 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1 ]
        expected_15 =
          Tuple 17
            [ 15
            , 46
            , 23
            , 70
            , 35
            , 106
            , 53
            , 160
            , 80
            , 40
            , 20
            , 10
            , 5
            , 16
            , 8
            , 4
            , 2
            , 1
            ]

      it "c = 11" do
        collatz 11 `shouldEqual` expected_11

      it "c = 15" do
        collatz 15 `shouldEqual` expected_15

  describe "Exercises Group - Monad Transformers" do
    describe "safeDivide" do
      it "should fail when dividing by zero" do
        (unwrap $ runExceptT $ safeDivide 5 0)
          `shouldEqual` (Left "Divide by zero!")

      it "should successfully divide for any other input" do
        (unwrap $ runExceptT $ safeDivide 6 3) `shouldEqual` (Right 2)

    describe "parser" do
      let
        runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

      it "should parse a string" do
        runParser (string "abc") "abcdef" `shouldEqual`
          (Right (Tuple (Tuple "abc" "def") [ "The state is abcdef" ]))

      it "should fail if string could not be parsed" do
        runParser (string "abc") "foobar"
          `shouldEqual` (Left [ "Could not parse" ])

    describe "indents with ReaderT and WriterT" do
      let
        expectedText =
          "Here is some indented text:\n\
          \  I am indented\n\
          \  So am I\n\
          \    I am even more indented"

      it "should render with indentations" do
        ( render' $ do
            line' "Here is some indented text:"
            indent' $ do
              line' "I am indented"
              line' "So am I"
              indent' $ do
                line' "I am even more indented"
        ) `shouldEqual` expectedText

  describe "Exercises Group - Monad Comprehensions/backtracking" do
    describe "parser" do
      let
        runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

      it "should parse as followed by bs" do
        runParser asFollowedByBs "aaabbcde" `shouldEqual`
          ( Right
              ( Tuple (Tuple "aaabb" "cde")
                  [ "The state is aaabbcde"
                  , "The state is aabbcde"
                  , "The state is abbcde"
                  , "The state is bbcde"
                  , "The state is bcde"
                  ]
              )
          )

      it "should fail if first is not a" do
        runParser asFollowedByBs "bfoobar"
          `shouldEqual` (Left [ "Could not parse" ])

      it "should parse as and bs" do
        runParser asOrBs "babbaacde" `shouldEqual`
          ( Right
              ( Tuple (Tuple "babbaa" "cde")
                  [ "The state is babbaacde"
                  , "The state is abbaacde"
                  , "The state is bbaacde"
                  , "The state is baacde"
                  , "The state is aacde"
                  , "The state is acde"
                  ]
              )
          )

      it "should fail if first is not a or b" do
        runParser asOrBs "foobar"
          `shouldEqual` (Left [ "Could not parse", "Could not parse" ])

  describe "Exercises Group - The RWS Monad" do
    let
      runGame
        :: Game Unit
        -> Either (List String) (RWSResult GameState Unit (List String))
      runGame testGame = runExcept $ runRWST testGame env initialGameState

      env = GameEnvironment
        { cheatMode: false, debugMode: false, playerName: "Phil" }

      playerHasAllItems (GameState { inventory }) = inventory == S.fromFoldable
        [ Candle, Matches ]

      mapIsEmpty (GameState { items }) = M.isEmpty items

      expectedLogs =
        ("You now have the Candle" : "You now have the Matches" : L.Nil)

    describe "adds all items to your inventory when cheating" do
      let
        runCheatTest label testGame =
          it label
            case runGame testGame of
              Left _ -> fail "game failed"
              Right (RWSResult actualState _ log) -> do
                playerHasAllItems actualState `shouldEqual` true
                mapIsEmpty actualState `shouldEqual` true
                L.sort log `shouldEqual` expectedLogs

      runCheatTest "only cheat" cheat
      runCheatTest "move and cheat" $ move 0 (-1) *> move 0 1 *> cheat
      runCheatTest "pickup matches and cheat" $ pickUp Matches *> cheat
      runCheatTest "pickup all, move, and cheat"
        $ pickUp Matches *> move 0 1 *> pickUp Candle *> cheat
