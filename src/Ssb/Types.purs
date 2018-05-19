module Ssb.Types where

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
