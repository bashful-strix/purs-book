module Test.Cp7.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (test)
import Data.Validation.Semigroup (invalid)

import Effect (Effect)

import Test.Spec (describe, it, parallel, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp7.Data.AddressBook (address)

import Test.Cp7.Solutions
  ( addMaybe
  , divMaybe
  , mulMaybe
  , subMaybe
  , addApply
  , divApply
  , mulApply
  , subApply
  , combineMaybe

  , stateRegex
  , nonEmptyRegex
  , validateAddressImproved
  )

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] $ parallel do
  describe "Chapter Examples" do
    pending "Todo for book maintainers - Add tests for chapter examples"

  describe "Exercise Group - Applicative and Effects" do
    describe "Exercise - Numeric operators that work with Maybe" do
      describe "addMaybe" do
        it "Just" do
          addMaybe (Just 5) (Just 2) `shouldEqual` Just 7

        it "Nothing on left" do
          addMaybe Nothing (Just 2) `shouldEqual` Nothing

        it "Nothing on right" do
          addMaybe (Just 5) Nothing `shouldEqual` Nothing

      it "subMaybe" do
        subMaybe (Just 5) (Just 2) `shouldEqual` Just 3

      it "mulMaybe" do
        mulMaybe (Just 5) (Just 2) `shouldEqual` Just 10

      it "divMaybe" do
        divMaybe (Just 5) (Just 2) `shouldEqual` Just 2

    describe "Exercise - Numeric operators that work with Apply" do
      describe "addApply" do
        it "Maybe Just" do
          addApply (Just 5) (Just 2) `shouldEqual` Just 7

        it "Maybe Nothing" do
          addApply (Just 5) Nothing `shouldEqual` Nothing

        it "Either Right" do
          addApply (Right 5) (Right 2)
            `shouldEqual` (Right 7 :: Either String Int)

        it "Either Left" do
          addApply (Right 5) (Left "fail")
            `shouldEqual` (Left "fail" :: Either String Int)

      describe "subApply" do
        it "Maybe" do
          subApply (Just 5) (Just 2) `shouldEqual` Just 3

        it "Either" do
          subApply (Right 5) (Right 2)
            `shouldEqual` (Right 3 :: Either String Int)

      describe "mulApply" do
        it "Maybe" do
          mulApply (Just 5) (Just 2) `shouldEqual` Just 10

        it "Either" do
          mulApply (Right 5) (Right 2)
            `shouldEqual` (Right 10 :: Either String Int)

      describe "divApply" do
        it "Maybe" do
          divApply (Just 5) (Just 2) `shouldEqual` Just 2

        it "Either" do
          divApply (Right 5) (Right 2)
            `shouldEqual` (Right 2 :: Either String Int)

    describe "Exercise - combineMaybe" do
      describe "Array Int" do
        it "Just" do
          combineMaybe (Just $ [ 1, 2, 3 ])
            `shouldEqual` [ Just 1, Just 2, Just 3 ]

        it "Nothing" do
          combineMaybe (Nothing :: Maybe (Array Int))
            `shouldEqual` [ Nothing ]

      describe "List Char" do
        it "Just" do
          combineMaybe (Just $ 'a' : 'b' : 'c' : Nil)
            `shouldEqual` (Just 'a' : Just 'b' : Just 'c' : Nil)

        it "Nothing" do
          combineMaybe (Nothing :: Maybe (List Char))
            `shouldEqual` (Nothing : Nil)

  describe "Exercise Group - Applicative Validation" do
    describe "Exercise - stateRegex" do
      let
        stateTest str exp = it str do
          test stateRegex str `shouldEqual` exp

      stateTest "CA" true
      stateTest "Ca" true
      stateTest "C" false
      stateTest "CAA" false
      stateTest "C3" false
      stateTest "C$" false

    describe "Exercise - nonEmptyRegex" do
      let
        nonEmptyTest str exp = it (show str) do
          test nonEmptyRegex str `shouldEqual` exp

      nonEmptyTest "Houston" true
      nonEmptyTest "My Street" true
      nonEmptyTest "Ñóñá" true
      nonEmptyTest " Start with whitespace" true
      nonEmptyTest "End with whitespace " true
      nonEmptyTest "" false
      nonEmptyTest " " false
      nonEmptyTest "\t" false

    describe "Exercise - validateAddressImproved" do
      it "Valid" do
        let addr = address "22 Fake St" "Fake City" "CA"

        validateAddressImproved addr `shouldEqual` pure addr

      it "Invalid Street" do
        (validateAddressImproved $ address "" "Fake City" "CA")
          `shouldEqual`
            invalid [ "Field 'Street' did not match the required format" ]

      it "Invalid City" do
        (validateAddressImproved $ address "22 Fake St" "\t" "CA")
          `shouldEqual`
            invalid [ "Field 'City' did not match the required format" ]

      it "Invalid State" do
        (validateAddressImproved $ address "22 Fake St" "Fake City" "C3")
          `shouldEqual`
            invalid [ "Field 'State' did not match the required format" ]
