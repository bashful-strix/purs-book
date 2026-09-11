module Test.Cp4.Main where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp4.ChapterExamples
  ( factorial
  , binomial
  , pascal

  , sameCity
  , fromSingleton

  , Amp(..)
  , Volt(..)
  , Watt(..)
  , calculateWattage
  )
import Cp4.Data.Person (Person)
import Cp4.Data.Picture
  ( Picture
  , Shape(..)
  , origin
  , getCentre

  , circleAtOrigin
  , doubleScaleAndCentre
  , shapeText
  )

john :: Person
john =
  { name: "John Smith"
  , address:
      { street: "123 Test Lane"
      , city: "Los Angeles"
      }
  }

rose :: Person
rose =
  { name: "Rose Jackson"
  , address:
      { street: "464 Sample Terrace"
      , city: "Los Angeles"
      }
  }

amy :: Person
amy =
  { name: "Amy Lopez"
  , address:
      { street: "10 Purs Street"
      , city: "Omaha"
      }
  }

samplePicture :: Picture
samplePicture =
  [ Circle origin 2.0
  , Circle { x: 2.0, y: 2.0 } 3.0
  , Rectangle { x: 5.0, y: 5.0 } 4.0 4.0
  ]

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Exercise Group - Simple Pattern Matching" do
    it "Exercise - factorial" do
      factorial 0 `shouldEqual` 1
      factorial 1 `shouldEqual` 1
      factorial 4 `shouldEqual` 24
      factorial 10 `shouldEqual` 3628800

    it "Exercise - binomial" do
      binomial 10 0 `shouldEqual` 1
      binomial 0 3 `shouldEqual` 0
      binomial 2 5 `shouldEqual` 0
      binomial 10 5 `shouldEqual` 252
      binomial 5 5 `shouldEqual` 1

    it "Exercise - pascal" do
      pascal 10 0 `shouldEqual` 1
      pascal 0 3 `shouldEqual` 0
      pascal 2 5 `shouldEqual` 0
      pascal 10 5 `shouldEqual` 252
      pascal 5 5 `shouldEqual` 1

  describe "Exercise Group - Array and Record Patterns" do
    it "Exercise - sameCity" do
      sameCity john rose `shouldEqual` true
      sameCity amy rose `shouldEqual` false
      sameCity
        { address: { city: 21, x: "" }, a: 0 }
        { address: { city: 21, y: [] }, b: 'a' }
        `shouldEqual` true

    it "Exercise - fromSingleton" do
      fromSingleton "default" [] `shouldEqual` "default"
      fromSingleton "default" [ "B" ] `shouldEqual` "B"
      fromSingleton "default" [ "B", "C", "D" ] `shouldEqual` "default"

  describe "Exercise Group - Algebraic Data Types" do
    it "Exercise - circleAtOrigin" do
      getCentre circleAtOrigin `shouldEqual` origin

    it "Exercise - doubleScaleAndCentre" do
      doubleScaleAndCentre (Circle origin 5.0)
        `shouldEqual` (Circle origin 10.0)
      doubleScaleAndCentre (Circle { x: 2.0, y: 2.0 } 5.0)
        `shouldEqual` (Circle origin 10.0)
      doubleScaleAndCentre (Rectangle { x: 0.0, y: 0.0 } 5.0 5.0)
        `shouldEqual` (Rectangle origin 10.0 10.0)
      doubleScaleAndCentre (Rectangle { x: 30.0, y: 30.0 } 20.0 20.0)
        `shouldEqual` (Rectangle origin 40.0 40.0)
      doubleScaleAndCentre (Line { x: -2.0, y: -2.0 } { x: 2.0, y: 2.0 })
        `shouldEqual` (Line { x: -4.0, y: -4.0 } { x: 4.0, y: 4.0 })
      doubleScaleAndCentre (Line { x: 0.0, y: 4.0 } { x: 4.0, y: 8.0 })
        `shouldEqual` (Line { x: -4.0, y: -4.0 } { x: 4.0, y: 4.0 })
      doubleScaleAndCentre (Text { x: 4.0, y: 6.0 } "Hello .purs!")
        `shouldEqual` (Text { x: 0.0, y: 0.0 } "Hello .purs!")

    it "Exercise - shapeText" do
      shapeText (Text origin "Hello .purs!")
        `shouldEqual` (Just "Hello .purs!")
      shapeText (Circle origin 1.0) `shouldEqual` Nothing
      shapeText (Rectangle origin 1.0 1.0) `shouldEqual` Nothing
      shapeText (Line origin { x: 1.0, y: 1.0 }) `shouldEqual` Nothing
  describe "Exercise Group - Newtype" do
    it "Exercise - calculateWattage" do
      let (Watt w) = calculateWattage (Amp 0.5) (Volt 120.0)
      w `shouldEqual` 60.0
