module Test.Cp2.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp2.Euler (answer, circleArea, diagonal, leftoverCents)

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

  describe "circleArea" do
    it "radius 1" do
      circleArea 1.0 `shouldEqual` 3.141592653589793

    it "radius 3" do
      circleArea 3.0 `shouldEqual` 28.274333882308138

  describe "leftoverCents" do
    it "23" do
      leftoverCents 23 `shouldEqual` 23

    it "456" do
      leftoverCents 456 `shouldEqual` 56

    it "-789" do
      leftoverCents (-789) `shouldEqual` (-89)
