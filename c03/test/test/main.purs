module Test.Cp3.Main where

import Prelude

import Data.Maybe (Maybe(..))

import Effect (Effect)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Cp3.Data.AddressBook
  ( AddressBook
  , Entry
  , emptyBook
  , insertEntry

  , findEntryByStreet
  , isInBook
  , removeDuplicates
  )

john :: Entry
john =
  { firstName: "John"
  , lastName: "Smith"
  , address:
      { street: "123 Fake St.", city: "Faketown", state: "CA" }
  }

peggy :: Entry
peggy =
  { firstName: "Peggy"
  , lastName: "Hill"
  , address:
      { street: "84 Rainey St.", city: "Arlen", state: "TX" }
  }

ned :: Entry
ned =
  { firstName: "Ned"
  , lastName: "Flanders"
  , address:
      { street: "740 Evergreen Terrace", city: "Springfield", state: "USA" }
  }

book :: AddressBook
book =
  insertEntry john
    $ insertEntry peggy
    $ insertEntry ned
        emptyBook

otherJohn :: Entry
otherJohn =
  { firstName: "John"
  , lastName: "Smith"
  , address:
      { street: "678 Fake Rd.", city: "Fakeville", state: "NY" }
  }

bookWithDuplicate :: AddressBook
bookWithDuplicate =
  insertEntry john
    $ insertEntry otherJohn
        book

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Exercise - findEntryByStreet" do
    it "Lookup existing" do
      (findEntryByStreet john.address.street book) `shouldEqual` (Just john)

    it "Lookup missing" do
      (findEntryByStreet "456 Nothing St." book) `shouldEqual` Nothing

  describe "Exercise - isInBook" do
    it "Check existing" do
      (isInBook ned.firstName ned.lastName book) `shouldEqual` true

    it "Check missing" do
      (isInBook "unknown" "person" book) `shouldEqual` false

  describe "Exercise - removeDuplicates" do
    it "Check duplicates" do
      (removeDuplicates bookWithDuplicate) `shouldEqual` book
