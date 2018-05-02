module App.IgoMsg where

import Prelude

import App.Common (getClient', messageTypeString)
import App.Utils (lookup', lookup_, maybeToEither, toObject')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (JObject, Json, fromObject, fromString, toNumber, toObject, toString)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Unfoldable (fromMaybe)
import Debug.Trace (traceAnyA)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Ssb.Client (close, getClient, publish, publishPrivate)
import Ssb.Config (SSB, Config, defaultConfig)

type SE eff a = Eff (ssb :: SSB | eff) a
type SA eff a = Aff (ssb :: SSB | eff) a

data IgoMsg
  = RequestMatch GameTerms
  | ExpireRequest
  | OfferMatch {|OfferMatchRows}
  | WithdrawOffer MsgKey
  | AcceptMatch MsgKey
  | DeclineMatch MsgKey
  | PlayMove { position :: BoardPosition, lastMove :: MsgKey, subjectiveMoveNum :: Int }
  | Kibitz { move :: MsgKey, text :: String }

type OfferMatchRows = (terms :: GameTerms, userKey :: UserKey, opponentColor :: StoneColor)

data SsbMessage = SsbMessage IgoMsg
  { key :: String
  , previous :: Maybe String
  , author :: String
  , timestamp :: Number  -- inner timestamp
  , hash :: String
  , signature :: String
  }

type MsgKey = String

type UserKey = String

type GameTerms =
  { size :: Int
  , komi :: Number
  , handicap :: Int
  }

data StoneColor = White | Black

data BoardPosition = BoardPosition Int Int

derive instance genericIgoMsg :: Generic IgoMsg
derive instance genericStoneColor :: Generic StoneColor
derive instance genericBoardPosition :: Generic BoardPosition

publishMsg :: ∀ eff. IgoMsg -> SA eff Unit
publishMsg msg = do
  client <- getClient'
  _ <- publish client $ toJson msg
  pure unit

publishPrivateMsg :: ∀ eff. IgoMsg -> Array UserKey -> SA eff Unit
publishPrivateMsg msg recips = do
  client <- getClient'
  _ <- publishPrivate client (toJson msg) recips
  pure unit

toJson :: IgoMsg -> Json
toJson msg = case toObject' $ encodeJson msg of
  Right o -> fromObject $ M.insert "type" (fromString messageTypeString) o
  Left err -> unsafePartial $ crashWith err

stripType :: Json -> Either String Json
stripType json = do
  o :: JObject <- toObject' json
  msgType :: String <- maybeToEither "no type specified" $ toString =<< M.lookup "type" o
  if msgType == messageTypeString
    then Right $ fromObject $ M.delete "type" o
    else Left "not an ssb-igo message"

parseMessage :: Json -> Either String SsbMessage
parseMessage json = do
  o <- toObject' json
  key <- lookup' "key" toString o
  value <- lookup' "value" toObject o

  previous <- toString <$> lookup_ "previous" value
  author <- lookup' "author" toString value
  timestamp <- lookup' "timestamp" (toNumber) value
  hash <- lookup' "hash" toString value
  signature <- lookup' "signature" toString value
  content <- lookup_ "content" value

  msg <- decodeJson =<< stripType content
  pure $ SsbMessage msg {key, previous, author, timestamp, hash, signature}


demoMsg :: IgoMsg
demoMsg = RequestMatch
  { size: 19
  , handicap: 0
  , komi: 5.5
  }

demoPrivate :: IgoMsg
demoPrivate = OfferMatch
  { userKey: "PhgZSAy4aWPYx231rgypWz8jjNOJmwCi9diVYiYHh50=.ed25519"
  , opponentColor: Black
  , terms:
    { size: 19
    , handicap: 0
    , komi: 5.5
    }
  }
