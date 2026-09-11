module Test.Cp6.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it, parallel, pending)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp6.Solutions
  ( Point(..)

  , Complex(..)
  , Shape(..)
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] $ parallel do
  describe "Chapter Examples" do
    pending "Todo for book maintainers - Add tests for chapter examples"

  describe "Show Me!" do
    it "Show Point" do
      (show $ Point { x: 1.0, y: 2.0 }) `shouldEqual` "(1.0, 2.0)"

  describe "Common Type Classes" do
    let cpx real imaginary = Complex { real, imaginary }

    describe "Show Complex" do
      it "possitve imaginary" do
        (show $ cpx 1.0 2.0) `shouldEqual` "1.0+2.0i"

      it "negative imaginary" do
        (show $ cpx 1.0 (-2.0)) `shouldEqual` "1.0-2.0i"

    describe "Eq Complex" do
      it "equal" do
        cpx 1.0 2.0 `shouldEqual` cpx 1.0 2.0

      it "not equal" do
        cpx 1.0 2.0 `shouldNotEqual` cpx 5.0 2.0

    describe "Semiring Complex" do
      it "add" do
        add (cpx 1.0 2.0) (cpx 3.0 4.0) `shouldEqual` (cpx 4.0 6.0)

      let v = cpx 1.2 3.4
      it "add zero" do
        add v zero `shouldEqual` v

      it "multiply" do
        mul (cpx 1.0 2.0) (cpx 3.0 4.0) `shouldEqual` (cpx (-5.0) 10.0)

      it "multiply one" do
        mul v one `shouldEqual` v

    describe "Ring Complex" do
      it "subtract" do
        sub (cpx 3.0 5.0) (cpx 1.0 2.0) `shouldEqual` (cpx 2.0 3.0)

    describe "Show Shape" do
      it "circle" do
        (show $ Circle (Point { x: 1.0, y: 2.0 }) 3.0)
          `shouldEqual` "(Circle (1.0, 2.0) 3.0)"

      it "rectangle" do
        (show $ Rectangle (Point { x: 1.0, y: 2.0 }) 3.0 4.0)
          `shouldEqual` "(Rectangle (1.0, 2.0) 3.0 4.0)"

      it "line" do
        (show $ Line (Point { x: 1.0, y: 2.0 }) (Point { x: 3.0, y: 4.0 }))
          `shouldEqual` "(Line (1.0, 2.0) (3.0, 4.0))"

      it "text" do
        (show $ Text (Point { x: 1.0, y: 2.0 }) "Hello")
          `shouldEqual` "(Text (1.0, 2.0) \"Hello\")"
