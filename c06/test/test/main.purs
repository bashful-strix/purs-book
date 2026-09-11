module Test.Cp6.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, parallel, pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] $ parallel do
  describe "Chapter Examples" do
    pending "Todo for book maintainers - Add tests for chapter examples"
