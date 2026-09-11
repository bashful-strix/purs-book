module Test.Cp11.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp11.Solutions (testParens)

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
