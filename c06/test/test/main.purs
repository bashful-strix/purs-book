module Test.Cp6.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it, parallel, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp6.Solutions
  ( Point(..)
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] $ parallel do
  describe "Chapter Examples" do
    pending "Todo for book maintainers - Add tests for chapter examples"

  describe "Show Me!" do
    it "Show Point" do
      (show $ Point { x: 1.0, y: 2.0 }) `shouldEqual` "(1.0, 2.0)"
