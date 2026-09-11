module Test.Cp8.Main where

import Prelude

import Data.List (List(..), (:), foldM)
import Data.Maybe (Maybe(..))

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp8.Examples (countThrows, safeDivide)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Chapter Examples" do
    describe "countThrows" do
      it "10" do
        countThrows 10 `shouldEqual` [ [ 4, 6 ], [ 5, 5 ], [ 6, 4 ] ]

      it "12" do
        countThrows 12 `shouldEqual` [ [ 6, 6 ] ]

    describe "safeDivide" do
      it "Just" do
        safeDivide 10 2 `shouldEqual` Just 5

      it "Nothing" do
        safeDivide 10 0 `shouldEqual` Nothing

    describe "foldM with safeDivide" do
      it "[5, 2, 2] has a Just answer" do
        foldM safeDivide 100 (5 : 2 : 2 : Nil) `shouldEqual` Just 5

      it "[5, 0, 2] has a Nothing answer" do
        foldM safeDivide 100 (5 : 0 : 2 : Nil) `shouldEqual` Nothing
