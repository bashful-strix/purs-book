module Test.Cp11.Main where

import Prelude

import Control.Monad.Writer (execWriter)

import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp11.Solutions
  ( testParens

  , line
  , indent
  , cat
  , render

  , sumArrayWriter
  , collatz
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
