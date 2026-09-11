module Test.Cp8.Main where

import Prelude

import Data.Either (fromLeft, fromRight)
import Data.List (List(..), (:), foldM)
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (error, message, try)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.Cp8.Examples (countThrows, safeDivide)
import Test.Cp8.Solutions
  ( third
  , possibleSums
  , filterM

  , exceptionDivide
  , estimatePi
  , fibonacci
  )

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

  describe "Exercises Group - Monads and Applicatives" do
    describe "third" do
      it "No elements" $
        third ([] :: Array Int) `shouldEqual` Nothing

      it "1 element" $
        third [ 1 ] `shouldEqual` Nothing

      it "2 elements" $
        third [ 1, 2 ] `shouldEqual` Nothing

      it "3 elements" $
        third [ 1, 2, 3 ] `shouldEqual` (Just 3)

      it "4 elements" $
        third [ 1, 2, 4, 3 ] `shouldEqual` (Just 4)

    describe "possibleSums" do
      it "[]" $
        possibleSums [] `shouldEqual` [ 0 ]

      it "[1, 2, 10]" $
        possibleSums [ 1, 2, 10 ]
          `shouldEqual` [ 0, 1, 2, 3, 10, 11, 12, 13 ]

    describe "filterM" do
      describe "Array Monad" do
        let
          onlyPositives :: Int -> Array Boolean
          onlyPositives i = [ i >= 0 ]

        it "Empty" $
          (filterM onlyPositives Nil) `shouldEqual` [ Nil ]

        it "Not Empty" $
          ( filterM
              onlyPositives
              (2 : (-1) : 4 : Nil)
          ) `shouldEqual` [ (2 : 4 : Nil) ]

      describe "Maybe Monad" do
        let
          -- This is an impractical filtering function,
          -- and could be simplified, but it's fine for
          -- testing purposes.
          onlyPositiveEvenIntegers :: Int -> Maybe Boolean
          onlyPositiveEvenIntegers i =
            if i < 0 then Nothing else Just $ 0 == i `mod` 2

        it "Nothing" $
          ( filterM
              onlyPositiveEvenIntegers
              (2 : 3 : (-1) : 4 : Nil)
          ) `shouldEqual` Nothing

        it "Just positive even integers" $
          ( filterM
              onlyPositiveEvenIntegers
              (2 : 3 : 4 : Nil)
          ) `shouldEqual` (Just (2 : 4 : Nil))

    describe "exceptionDivide" do
      it "6 / 3" $
        ( fromRight 0
            $ unsafePerformEffect
            $ try $ exceptionDivide 6 3
        ) `shouldEqual` 2

      it "6 / 0" $
        ( message
            $ fromLeft (error "")
            $ unsafePerformEffect
            $ try $ exceptionDivide 6 0
        ) `shouldEqual` "div zero"

    describe "ST" do
      describe "estimatePi" do
        it "1000 terms of Gregory Series" $
          (abs (estimatePi 1000 - pi) < 0.002) `shouldEqual` true

        it "1000000 terms of Gregory Series" $
          (abs (estimatePi 1000000 - pi) < 0.000002) `shouldEqual` true

      describe "fibonacci" do
        it "40th Fibonacci number" $
          (fibonacci 40) `shouldEqual` 102334155

        it "45th Fibonacci number" $
          (fibonacci 45) `shouldEqual` 1134903170
