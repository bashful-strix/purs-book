module Test.Cp4.Main where

import Prelude

import Effect (Effect)

import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

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
  pending "tests"
