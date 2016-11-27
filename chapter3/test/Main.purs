module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."


-- module Test.Main where
--
-- import Prelude
--
-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, logShow)
-- import Data.AddressBook (AddressBook, Address (), Entry, emptyBook, insertEntry, findEntry, showEntry)
-- import Data.Maybe (Maybe)
--
-- fakeAddr :: Address
-- fakeAddr = Address { street: "123 Fake St."
--                    , city: "Faketown"
--                    , state: "CA"
--                    }
--
-- example :: Entry
-- example =
--   { firstName: "John"
--   , lastName: "Smith"
--   , address: fakeAddr
--   }
--
-- book0 :: AddressBook
-- book0 = emptyBook
--
-- printEntry :: String -> String -> AddressBook -> Maybe String
-- printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book
--
-- main :: Eff (console :: CONSOLE) Unit
-- main = do
--   let book1 = insertEntry example emptyBook
--
--   logShow $ printEntry "John" "Smith" book0
--   logShow $ printEntry "John" "Smith" book1
