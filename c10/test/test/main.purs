module Test.Cp10.Main where

import Prelude

import Data.Argonaut (JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried (runFn2, runFn3)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn2)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp10.Examples
  ( Complex
  , Undefined
  , addComplex
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
  , undefinedHead
  , unsafeHead
  , yell
  )
import Test.Cp10.URI (_encodeURIComponent)

import Test.Cp10.Solutions
  ( volumeFn
  , volumeArrow

  , cumulativeSumsComplex

  , quadraticRoots
  , toMaybe

  , valuesOfMap
  , valuesOfMapGeneric
  , quadraticRootsSet
  , quadraticRootsSafe
  , parseAndDecodeArray2D
  , Tree(..)
  , IntOrString(..)
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

  describe "Exercise Group - Passing Simple Types" do
    describe "Exercise - cumulativeSumsComplex" do
      it "sequential" do
        cumulativeSumsComplex
          [ { real: 1.0, imag: 2.0 }
          , { real: 3.0, imag: 4.0 }
          , { real: 5.0, imag: 6.0 }
          ]
          `shouldEqual`
            [ { real: 1.0, imag: 2.0 }
            , { real: 4.0, imag: 6.0 }
            , { real: 9.0, imag: 12.0 }
            ]

  describe "Exercise Group - Beyond Simple Types" do
    describe "Exercise - quadraticRoots" do
      let
        helper testName poly r1 r2 =
          it testName do
            (orderCpx $ quadraticRoots poly)
              `shouldEqual` (orderCpx $ Pair r1 r2)

      helper "Real"
        { a: 1.0, b: 2.0, c: -3.0 }
        { real: 1.0, imag: 0.0 }
        { real: -3.0, imag: 0.0 }

      helper "Imaginary"
        { a: 4.0, b: 0.0, c: 16.0 }
        { real: 0.0, imag: 2.0 }
        { real: 0.0, imag: -2.0 }

      helper "Complex"
        { a: 2.0, b: 2.0, c: 5.0 }
        { real: -0.5, imag: 1.5 }
        { real: -0.5, imag: -1.5 }

      helper "Repeated"
        { a: 3.0, b: -6.0, c: 3.0 }
        { real: 1.0, imag: 0.0 }
        { real: 1.0, imag: 0.0 }

    describe "Exercise - toMaybe" do
      it "Nothing" do
        (toMaybe $ (undefinedHead [] :: Undefined Int)) `shouldEqual` Nothing

      it "Just" do
        (toMaybe $ undefinedHead [1]) `shouldEqual` (Just 1)

  describe "Exercise Group - JSON" do
    describe "Exercise - valuesOfMap" do
      it "Items" do
        (valuesOfMap $ Map.fromFoldable [ Tuple "hat" 1, Tuple "cat" 2 ])
          `shouldEqual` (Right $ Set.fromFoldable [ 1, 2 ])

      it "Empty" do
        (valuesOfMap $ Map.fromFoldable [])
          `shouldEqual` (Right $ Set.fromFoldable [])

    describe "Exercise - valuesOfMapGeneric" do
      it "String Int" do
        (valuesOfMapGeneric $ Map.fromFoldable [ Tuple "hat" 1, Tuple "cat" 2 ])
          `shouldEqual` (Right $ Set.fromFoldable [ 1, 2 ])

      it "(Array Int) String" do
        ( valuesOfMapGeneric
          $ Map.fromFoldable [ Tuple [ 1, 3, 5 ] "hat", Tuple [ 43, 8 ] "cat" ]
        ) `shouldEqual` (Right $ Set.fromFoldable [ "hat", "cat" ])

    describe "Exercise - quadraticRootsSet" do
      let
        helper testName poly r1 r2 =
          it testName do
            quadraticRootsSet poly
              `shouldEqual` (Right $ Set.fromFoldable [ r1, r2 ])

      helper "Real"
        { a: 1.0, b: 2.0, c: -3.0 }
        { real: 1.0, imag: 0.0 }
        { real: -3.0, imag: 0.0 }

      helper "Imaginary"
        { a: 4.0, b: 0.0, c: 16.0 }
        { real: 0.0, imag: 2.0 }
        { real: 0.0, imag: -2.0 }

      helper "Complex"
        { a: 2.0, b: 2.0, c: 5.0 }
        { real: -0.5, imag: 1.5 }
        { real: -0.5, imag: -1.5 }

      helper "Repeated"
        { a: 3.0, b: -6.0, c: 3.0 }
        { real: 1.0, imag: 0.0 }
        { real: 1.0, imag: 0.0 }

    describe "Exercise - quadraticRootsSafe" do
      let
        helper testName poly r1 r2 =
          it testName do
            (map orderCpx $ quadraticRootsSafe poly)
              `shouldEqual` (Right $ orderCpx $ Pair r1 r2)

      helper "Real"
        { a: 1.0, b: 2.0, c: -3.0 }
        { real: 1.0, imag: 0.0 }
        { real: -3.0, imag: 0.0 }

      helper "Imaginary"
        { a: 4.0, b: 0.0, c: 16.0 }
        { real: 0.0, imag: 2.0 }
        { real: 0.0, imag: -2.0 }

      helper "Complex"
        { a: 2.0, b: 2.0, c: 5.0 }
        { real: -0.5, imag: 1.5 }
        { real: -0.5, imag: -1.5 }

      helper "Repeated"
        { a: 3.0, b: -6.0, c: 3.0 }
        { real: 1.0, imag: 0.0 }
        { real: 1.0, imag: 0.0 }

    it "Exercise - parseAndDecodeArray2D" do
      let
        arr = [ [ 1, 2, 3 ], [ 4, 5 ], [ 6 ] ]
      -- the correct JSON string happens to also be produced by show
      (parseAndDecodeArray2D $ show arr) `shouldEqual` (Right arr)

    it "Exercise - encode decode Tree" do
      let
        tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
      (decodeJson $ encodeJson tree) `shouldEqual` (Right tree)

    describe "Exercise - IntOrString" do
      it "IoS to IoS Int" do
        let
          ios = IntOrString_Int 1
        (decodeJson $ encodeJson ios) `shouldEqual` (Right ios)

      it "IoS to IoS String" do
        let
          ios = IntOrString_String "one"
        (decodeJson $ encodeJson ios) `shouldEqual` (Right ios)

      it "Int to IoS" do
        let
          int = 1
        (decodeJson $ encodeJson int)
          `shouldEqual` (Right $ IntOrString_Int int)

      it "String to IoS" do
        let
          str = "one"
        (decodeJson $ encodeJson str)
          `shouldEqual` (Right $ IntOrString_String str)

      it "IoS to Int" do
        let
          int = 1
        (decodeJson $ encodeJson $ IntOrString_Int int)
          `shouldEqual` (Right int)

      it "IoS to String" do
        let
          str = "one"
        (decodeJson $ encodeJson $ IntOrString_String str)
          `shouldEqual` (Right str)

      it "Neither, a Number instead" do
        let
          (decoded :: Either _ IntOrString) = decodeJson $ encodeJson 1.5
        isLeft decoded `shouldEqual` true

-- Put in ascending order by real, then imag components
orderCpx :: Pair Complex -> Pair Complex
orderCpx (Pair c1 c2)
  | c1.real < c2.real = Pair c1 c2
  | c1.real > c2.real = Pair c2 c1
  | c1.imag < c2.imag = Pair c1 c2
  | otherwise = Pair c2 c1
