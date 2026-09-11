module Cp3.Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry =
  entry.lastName <> ", " <>
  entry.firstName <> ": " <>
  showAddress entry.address

showAddress :: Address -> String
showAddress addr =
  addr.street <> ", " <>
  addr.city <> ", " <>
  addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry fn ln =
  -- head <<< filter (\entry -> entry.firstName == fn && entry.lastName == ln)
  head <<< filter ((_.firstName >>> eq fn) && (_.lastName >>> eq ln))
