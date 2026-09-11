module Test.Cp2.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp2.Euler (answer)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "answer" do
    it "below 10" do
      answer 10 `shouldEqual` 23

    it "below 1000" do
      answer 1000 `shouldEqual` 233168
