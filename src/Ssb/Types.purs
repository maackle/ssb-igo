module Ssb.Types where

import Data.Maybe (Maybe)

type MessageKey = String
type UserKey = String

newtype Cyphertext a = Cyphertext String

type SsbKeys =
  { id :: String
  , public :: String
  , private :: String
  }

foreign import data Plugin :: Type
foreign import data Sbot :: Type
type SbotConn = Sbot

type SsbMessagePayload =
  { key :: String
  , previous :: Maybe String
  , author :: String
  , timestamp :: Number  -- inner timestamp
  , hash :: String
  , signature :: String
  -- no content because we want to store that unmarshalled within an ADT
  }
