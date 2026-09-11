module Test.Cp4.Main where

import Prelude

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp4.ChapterExamples
  ( factorial
  , binomial
  , pascal
  )
import Cp4.Data.Person (Person)
import Cp4.Data.Picture
  ( Picture
  , Shape(..)
  , origin
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
