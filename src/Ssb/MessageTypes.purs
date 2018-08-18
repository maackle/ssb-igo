module Ssb.MessageTypes where

import Prelude

import App.Utils (lookup', lookup_, toArray', toObject')
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, Json, decodeJson, toArray, toNumber, toObject, toString)
import Data.Argonaut.Generic.Aeson as G
import Data.Array (head)
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
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

type AboutStreamMessage = (
  StrMap
    { name :: Maybe String
    , image :: Maybe String
    }
  )

getAboutMap :: Json -> String -> Either String AboutStreamMessage
getAboutMap json me = do
  o <- toObject' json
  r :: AboutStreamMessage <- sequence $ flip M.mapWithKey o $ \k j -> do
    m <- toObject' j
    let nameJson = do
                m <- toObject =<< M.lookup "name" m
                M.lookup me m <|> M.lookup k m
    let imageJson = do
                m <- toObject =<< M.lookup "image" m
                M.lookup me m <|> M.lookup k m
    let name = maybe Nothing (toString <=< head <=< toArray) nameJson
    let image = maybe Nothing (toString <=< head <=< toArray) imageJson
    pure {name, image}
  pure r
