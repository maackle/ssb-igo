module App.IgoMsg where

import Prelude

import App.Common (getClient', messageTypeString)
import App.Utils (lookup', lookup_, maybeToEither, toObject')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (JObject, Json, fromObject, fromString, toNumber, toObject, toString)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Either (Either(..), fromRight)
import Data.Generic (class Generic)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Unfoldable (fromMaybe)
import Debug.Trace (traceAnyA)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Ssb.Client (ClientConnection, close, getClient, publish, publishPrivate)
import Ssb.Config (SSB, Config)
import Ssb.Types (MessageKey, UserKey)

type SE eff a = Eff (ssb :: SSB | eff) a
type SA eff a = Aff (ssb :: SSB | eff) a
type MsgKey = MessageKey

data IgoMsg
  = RequestMatch RequestMatchPayload
  | ExpireRequest MsgKey
  | OfferMatch OfferMatchPayload
  | WithdrawOffer MsgKey
  | AcceptMatch AcceptMatchPayload
  | DeclineMatch DeclineMatchPayload
  | AcknowledgeDecline MsgKey
  | PlayMove PlayMovePayload
  | Kibitz KibitzPayload

type OfferMatchPayload =
  {terms :: GameTerms, myColor :: StoneColor, opponentKey :: String}
type RequestMatchPayload =
  {terms :: GameTerms}
type AcceptMatchPayload =
  {offerKey :: MsgKey, terms :: GameTerms}
type DeclineMatchPayload =
  {offerKey :: MsgKey, reason :: Maybe String}
type PlayMovePayload =
  { position :: BoardPosition, lastMove :: MsgKey, subjectiveMoveNum :: Int }
type KibitzPayload =
  { move :: MsgKey, text :: String }

data SsbMessage = SsbMessage IgoMsg
  { key :: String
  , previous :: Maybe String
  , author :: String
  , timestamp :: Number  -- inner timestamp
  , hash :: String
  , signature :: String
  }

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

publishMsg' :: ∀ eff. ClientConnection -> IgoMsg -> SA eff SsbMessage
publishMsg' client msg = do
  msg <- publish client $ toJson msg
  pure $ unsafePartial $ fromRight $ parseMessage msg

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
  { terms:
    { size: 19
    , handicap: 0
    , komi: 5.5
    }
  }

demoOfferPayload :: UserKey -> OfferMatchPayload
demoOfferPayload opponentKey =
  { myColor: Black
  , opponentKey
  , terms: demoTerms
  }

demoTerms =
  { size: 19
  , handicap: 0
  , komi: 5.5
  }
