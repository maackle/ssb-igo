module Ssb.MessageTypes where

import Prelude

import App.Utils (lookup', lookup_, toObject')
import Data.Argonaut (class DecodeJson, Json, decodeJson, toNumber, toObject, toString)
import Data.Argonaut.Generic.Aeson as G
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Ssb.Types (UserKey)


type SsbMessage a =
  { key :: String
  , previous :: Maybe String
  , author :: String
  , timestamp :: Number  -- inner timestamp
  , hash :: String
  , signature :: String
  , content :: a
  }

parsePayload :: Json -> Either String (SsbMessage Json)
parsePayload json = do
  o <- toObject' json
  key <- lookup' "key" toString o
  value <- lookup' "value" toObject o

  previous <- toString <$> lookup_ "previous" value
  author <- lookup' "author" toString value
  timestamp <- lookup' "timestamp" toNumber value
  hash <- lookup' "hash" toString value
  signature <- lookup' "signature" toString value
  content <- lookup_ "content" value

  pure $ {key, previous, author, timestamp, hash, signature, content}

type AboutContent =
  { about :: UserKey
  , name :: Maybe String
  }

newtype AboutContentN = AboutContentN AboutContent
derive instance newtypeAboutContentN :: Newtype AboutContentN _
derive instance genericAboutContentN :: Generic AboutContentN

newtype AboutMessage = AboutMessage (SsbMessage AboutContent)
-- derive instance genericAboutMessage :: Generic AboutMessage
instance decodeAboutMessage :: DecodeJson AboutMessage where
  decodeJson json = do
    payload@{content} <- parsePayload json
    about :: AboutContentN <- G.decodeJson content
    pure $ AboutMessage $ payload {content = unwrap about}
