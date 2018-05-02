module Ssb.Types where

type MessageKey = String
type UserKey = String

newtype Cyphertext a = Cyphertext String
