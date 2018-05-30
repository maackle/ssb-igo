module Ssb.Types where

import Prelude

import App.Utils (lookup', lookup_, toObject')
import Data.Argonaut (Json, toNumber, toObject, toString)
import Data.Either (Either)
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
