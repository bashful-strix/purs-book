module Test.Cp11.Main where

import Prelude

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
