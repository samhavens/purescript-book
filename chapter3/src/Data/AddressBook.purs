module Data.AddressBook where

import Control.Plus (empty)
import Data.Generic (class Generic, gEq, gShow)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isNothing)
import Prelude (class Eq, class Show, show, (<<<), (==), (&&), (<>), ($), not)

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }
derive instance genericAddress :: Generic Address
instance showAddress :: Show Address where show = gShow
instance eqAddress :: Eq Address where eq = gEq

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  show entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter filterEntryByAddress
  where
  filterEntryByAddress :: Entry -> Boolean
  filterEntryByAddress entry = entry.address == address

nameInBook :: String -> String -> AddressBook -> Boolean
nameInBook firstName lastName = not isNothing <<< findEntry firstName lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy entryEquality
  where
  entryEquality x y = x.firstName == y.firstName && x.lastName == y.lastName
