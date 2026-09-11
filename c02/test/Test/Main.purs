module Test.Cp2.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp2.Euler
  ( answer

  , diagonal
  )

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "answer" do
    it "below 10" do
      answer 10 `shouldEqual` 23

    it "below 1000" do
      answer 1000 `shouldEqual` 233168

  describe "diagonal" do
    it "3 4 5" do
      diagonal 3.0 4.0 `shouldEqual` 5.0

    it "5 12 13" do
      diagonal 5.0 12.0 `shouldEqual` 13.0
