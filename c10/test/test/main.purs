module Test.Cp10.Main where

import Prelude

import Data.Argonaut (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn2)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp10.Examples
  ( addComplex
  , addComplexDecodedBroken
  , addComplexDecodedWorking
  , bold
  , cumulativeSums
  , cumulativeSumsDecodedBroken
  , cumulativeSumsDecodedWorking
  , curriedAdd
  , curriedSum
  , diagonal
  , diagonalArrow
  , diagonalAsync
  , diagonalLog
  , diagonalUncurried
  , isEmpty
  , mapSetFoo
  , maybeHead
  , showEquality
  , sleep
  , square
  , uncurriedAdd
  , uncurriedSum
  , unsafeHead
  , yell
  )
import Test.Cp10.URI (_encodeURIComponent)

import Test.Cp10.Solutions
  ( volumeFn
  , volumeArrow
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Chapter Examples" do
    it "uri" do
      _encodeURIComponent "Hello World" `shouldEqual` "Hello%20World"

    it "square" do
      square 5.0 `shouldEqual` 25.0

    it "diagonal" do
      diagonal 3.0 4.0 `shouldEqual` 5.0

    it "diagonalArrow" do
      diagonalArrow 3.0 4.0 `shouldEqual` 5.0

    it "diagonalUncurried" do
      runFn2 diagonalUncurried 3.0 4.0 `shouldEqual` 5.0

    it "uncurriedAdd" do
      runFn2 uncurriedAdd 3 10 `shouldEqual` 13

    it "uncurriedSum" do
      uncurriedSum `shouldEqual` 13

    it "curriedAdd" do
      curriedAdd 3 10 `shouldEqual` 13

    it "curriedSum" do
      curriedSum `shouldEqual` 13

    it "cumulativeSums" do
      cumulativeSums [ 1, 2, 3 ] `shouldEqual` [ 1, 3, 6 ]

    it "addComplex" do
      addComplex { real: 1.0, imag: 2.0 } { real: 3.0, imag: 4.0 } `shouldEqual`
        { imag: 6.0, real: 4.0 }

    it "maybeHead - Just" do
      maybeHead [ 1, 2, 3 ] `shouldEqual` (Just 1)

    it "maybeHead - Nothing" do
      maybeHead ([] :: Array Int) `shouldEqual` Nothing

    it "isEmpty - false" do
      isEmpty [ 1, 2, 3 ] `shouldEqual` false

    it "isEmpty - true" do
      isEmpty [] `shouldEqual` true

    -- It is not possible to test the thrown exception
    -- by catching with a `try` because `unsafeHead`
    -- lacks `Effect` in its return type.
    -- Lifting with `pure` doesn't help in this situation.
    it "unsafeHead - value" do
      unsafeHead [ 1, 2, 3 ] `shouldEqual` 1

    it "bold" do
      (bold $ Tuple 1 "Hat") `shouldEqual` "(TUPLE 1 \"HAT\")!!!"

    it "showEquality - not equal" do
      showEquality Nothing (Just 5) `shouldEqual`
        "Nothing is not equal to (Just 5)"

    it "showEquality - equivalent" do
      showEquality [ 1, 2 ] [ 1, 2 ] `shouldEqual` "Equivalent"

    -- Cannot test for actual logged value
    it "yell" do
      result <- liftEffect $ yell $ Tuple 1 "Hat"
      result `shouldEqual` unit

    it "diagonalLog" do
      result <- liftEffect $ runEffectFn2 diagonalLog 3.0 4.0
      result `shouldEqual` 5.0

    it "sleep" do
      result <- sleep 1
      result `shouldEqual` unit

    it "diagonalAsync" do
      result <- diagonalAsync 1 3.0 4.0
      result `shouldEqual` 5.0

    describe "cumulativeSums Json" do
      it "broken" do
        cumulativeSumsDecodedBroken [ 1, 2, 3 ]
          `shouldEqual`
            (Left $ Named "Array" $ AtIndex 3 $ TypeMismatch "Number")

      it "working" do
        cumulativeSumsDecodedWorking [ 1, 2, 3 ]
          `shouldEqual` (Right [ 1, 3, 6 ])

    describe "addComplex Json" do
      it "broken" do
        addComplexDecodedBroken
          { real: 1.0, imag: 2.0 }
          { real: 3.0, imag: 4.0 }
          `shouldEqual`
            (Left $ AtKey "imag" MissingValue)

      it "working" do
        addComplexDecodedWorking
          { real: 1.0, imag: 2.0 }
          { real: 3.0, imag: 4.0 }
          `shouldEqual`
            (Right { imag: 6.0, real: 4.0 })

    it "mapSetFoo" do
      mapSetFoo (Map.fromFoldable [ (Tuple "cat" 2), (Tuple "hat" 1) ])
        `shouldEqual`
          ( Right
              ( Map.fromFoldable
                  [ (Tuple "Foo" 42), (Tuple "cat" 2), (Tuple "hat" 1) ]
              )
          )

  describe "Exercise Group - Calling JavaScript" do
    describe "Exercise - volumeFn" do
      it "1 2 3" do
        runFn3 volumeFn 1.0 2.0 3.0 `shouldEqual` 6.0

      it "1 0 3" do
        runFn3 volumeFn 1.0 0.0 3.0 `shouldEqual` 0.0

    describe "Exercise - volumeArrow" do
      it "1 2 3" do
        volumeArrow 1.0 2.0 3.0 `shouldEqual` 6.0

      it "1 0 3" do
        volumeArrow 1.0 0.0 3.0 `shouldEqual` 0.0
