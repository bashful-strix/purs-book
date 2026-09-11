module Cp3.Data.AddressBook where

import Prelude

import Control.Plus (empty)

import Data.Lens (Lens')
import Data.Lens.Fold (anyOf, elemOf, findOf, folded)
import Data.Lens.Record (prop)

import Data.Function (on)
import Data.List (List(..), nubByEq)
import Data.Maybe (Maybe)

import Type.Proxy (Proxy(..))

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

_firstName :: Lens' Entry String
_firstName = prop (Proxy :: _ "firstName")

_lastName :: Lens' Entry String
_lastName = prop (Proxy :: _ "lastName")

_address :: Lens' Entry Address
_address = prop (Proxy :: _ "address")

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

_street :: Lens' Address String
_street = prop (Proxy :: _ "street")

_city :: Lens' Address String
_city = prop (Proxy :: _ "city")

_state :: Lens' Address String
_state = prop (Proxy :: _ "state")

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
  -- head <<< filter ((_.firstName >>> eq fn) && (_.lastName >>> eq ln))
  findOf folded ((_firstName `elemOf` fn) && (_lastName `elemOf` ln))

-- ex 1 {{{

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet st =
  -- head <<< filter (_.address.street >>> eq st)
  findOf folded ((_address <<< _street) `elemOf` st)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fn ln =
  -- not <<< null <<< filter ((_.firstName >>> eq fn) && (_.lastName >>> eq ln))
  -- not <<< null <<< filter ((eq fn <<< _.firstName) && (eq ln <<< _.lastName))
  -- isJust <<< findEntry fn ln
  anyOf folded ((_firstName `elemOf` fn) && (_lastName `elemOf` ln))

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  nubByEq ((eq `on` _.firstName) && (eq `on` _.lastName))

-- }}}
