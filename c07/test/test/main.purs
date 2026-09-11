module Test.Cp7.Main where

import Prelude

import Control.Monad.Writer (execWriter, tell)

import Data.Array ((..))
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr, foldMap)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (test)
import Data.Traversable (sequence, traverse)
import Data.Validation.Semigroup (invalid)

import Effect (Effect)

import Test.Spec (describe, it, parallel, pending)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp7.Data.AddressBook (PhoneType(..), address, phoneNumber)
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

  , Tree(..)
  , traversePreOrder
  , traversePostOrder
  , validatePersonOptionalAddress
  , traverseUsingSequence
  , sequenceUsingTraverse
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

  describe "Exercise Group - Traversable Functors" do
    let
      tree = Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)

      leaf :: forall a. a -> Tree a
      leaf x = Branch Leaf x Leaf

      intTree =
        Branch
          (Branch (leaf 1) 2 (leaf 3))
          4
          (Branch (leaf 5) 6 (leaf 7))

    describe "Exercise - Tree Show and Eq" do
      it "Show" do
        show tree `shouldEqual`
          "(Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf))"

      it "Eq - Equal" do
        tree `shouldEqual` tree

      it "Eq - Not Equal" do
        tree `shouldNotEqual` Leaf

    describe "Exercise - traverse" do
      describe "Functor Tree" do
        it "Functor - map" do
          map show intTree `shouldEqual`
            Branch
              (Branch (leaf "1") "2" (leaf "3"))
              "4"
              (Branch (leaf "5") "6" (leaf "7"))

      describe "Foldable Tree" do
        it "Foldable - foldr" $
          foldr (\x acc -> show x <> acc) "" intTree
            `shouldEqual` "1234567"

        it "Foldable - foldl" $
          foldl (\acc x -> show x <> acc) "" intTree
            `shouldEqual` "7654321"

        it "Foldable - foldMap" $
          foldMap (\x -> show x) intTree `shouldEqual` "1234567"

      describe "Maybe side-effect" do
        it "Just - traverse" $
          (traverse fromNumber $ Branch (leaf 1.0) 2.0 (leaf 3.0))
            `shouldEqual` (Just $ Branch (leaf 1) 2 (leaf 3))

        it "Just - sequence" $
          (sequence $ Branch (leaf $ Just 1) (Just 2) (leaf $ Just 3))
            `shouldEqual` (Just $ Branch (leaf 1) 2 (leaf 3))

        it "Nothing - traverse" $
          (traverse fromNumber $ Branch (leaf 1.0) 2.0 (leaf 3.7))
            `shouldEqual` Nothing

        it "Nothing - sequence" $
          (sequence $ Branch (leaf $ Nothing) (Just 2) (leaf $ Just 3))
            `shouldEqual` Nothing

      it "Array side-effect - check traversal order" $
        ( execWriter
            $ traverse (\x -> tell [ x ])
            $ Branch
                (Branch (leaf 1) 2 (leaf 3))
                4
                (Branch (leaf 5) 6 (leaf 7))
        ) `shouldEqual` (1 .. 7)

    it "Exercise - traversePreOrder" $
      ( execWriter
          $ traversePreOrder (\x -> tell [ x ])
          $ Branch
              (Branch (leaf 3) 2 (leaf 4))
              1
              (Branch (leaf 6) 5 (leaf 7))
      ) `shouldEqual` (1 .. 7)

    it "Exercise - traversePostOrder" $
      ( execWriter
          $ traversePostOrder (\x -> tell [ x ])
          $ Branch (Branch (leaf 1) 3 (leaf 2)) 7 (Branch (leaf 4) 6 (leaf 5))
      ) `shouldEqual` (1 .. 7)

    describe "Exercise - validatePersonOptionalAddress" do
      let
        examplePerson =
          { firstName: "John"
          , lastName: "Smith"
          , homeAddress: Just $ address "123 Fake St." "FakeTown" "CA"
          , phones:
              [ phoneNumber HomePhone "555-555-5555"
              , phoneNumber CellPhone "555-555-0000"
              ]
          }

      it "Just Address" do
        validatePersonOptionalAddress examplePerson `shouldEqual`
          pure examplePerson

      it "Nothing" do
        let
          examplePersonNoAddress = examplePerson { homeAddress = Nothing }
        validatePersonOptionalAddress examplePersonNoAddress
          `shouldEqual` pure examplePersonNoAddress

      it "Just Address with empty city" do
        ( validatePersonOptionalAddress $ examplePerson
            { homeAddress = Just $ address "123 Fake St." "" "CA" }
        ) `shouldEqual` invalid ([ "Field 'City' cannot be empty" ])

    describe "Exercise - sequenceUsingTraverse" do
      it "Just" do
        sequenceUsingTraverse [ Just 1, Just 2 ]
          `shouldEqual` Just [ 1, 2 ]

      it "Nothing" do
        sequenceUsingTraverse [ Just 1, Nothing ] `shouldEqual` Nothing

    describe "Exercise - traverseUsingSequence" do
      it "Just" do
        traverseUsingSequence fromNumber [ 1.0, 2.0 ]
          `shouldEqual` Just [ 1, 2 ]

      it "Nothing" do
        traverseUsingSequence fromNumber [ 1.0, 2.7 ]
          `shouldEqual` Nothing
