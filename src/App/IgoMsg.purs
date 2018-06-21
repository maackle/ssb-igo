module App.IgoMsg where

import Prelude

import App.Common (messageTypeString)
import App.Utils (maybeToEither, toObject')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (JObject, Json, fromObject, fromString, toString)
import Data.Argonaut.Generic.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), fromRight)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.StrMap as M
import Debug.Trace (traceAny)
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Ssb.Client (publish)
import Ssb.Config (SSB)
import Ssb.MessageTypes (SsbMessage, parsePayload)
import Ssb.Types (MessageKey, SbotConn, UserKey)

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
  {terms :: GameTerms, myColor :: StoneColor, opponentKey :: UserKey}
type RequestMatchPayload =
  {terms :: GameTerms}
type AcceptMatchPayload =
  {offerKey :: MsgKey, terms :: GameTerms}
type DeclineMatchFields =
  (offerKey :: MsgKey, reason :: Maybe String)
type DeclineMatchPayload = {|DeclineMatchFields}
type PlayMovePayload =
  { move :: IgoMove, lastMove :: MsgKey, subjectiveMoveNum :: Int }
type KibitzPayload =
  { move :: MsgKey, text :: String }

data IgoMove
  = PlayStone BoardPosition
  | Pass
  | Resign
  | ToggleDead BoardPosition
  | Finalize

type SsbIgoMsg = SsbMessage IgoMsg


type GameTerms =
  { size :: Int
  , komi :: Number
  , handicap :: Int
  }

data StoneColor = White | Black

newtype BoardPosition = BoardPosition BoardPositionData
type BoardPositionData = {x :: Int, y :: Int}

derive instance genericIgoMsg :: Generic IgoMsg
derive instance genericIgoMove :: Generic IgoMove
instance showIgoMove :: Show IgoMove where show = gShow
derive instance genericStoneColor :: Generic StoneColor
derive instance genericBoardPosition :: Generic BoardPosition
derive instance newtypeBoardPosition :: Newtype BoardPosition _

derive instance eqStoneColor :: Eq StoneColor

publishMsg :: ∀ eff. SbotConn -> IgoMsg -> SA eff Unit
publishMsg client msg = do
  _ <- publish client $ toJson msg
  pure unit

publishMsg' :: ∀ eff. SbotConn -> IgoMsg -> SA eff SsbIgoMsg
publishMsg' client msg = do
  msg <- publish client $ toJson msg
  case parseIgoMessage msg of
    Right m -> pure m
    Left err -> traceAny msg $ const $ unsafeCrashWith $ "can't parse: " <> err

-- publishPrivateMsg :: ∀ eff. IgoMsg -> Array UserKey -> SA eff Unit
-- publishPrivateMsg msg recips = do
--   client <- getClient'
--   _ <- publishPrivate client (toJson msg) recips
--   pure unit

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

parseIgoMessage :: Json -> Either String SsbIgoMsg
parseIgoMessage json = do
  payload :: SsbMessage Json <- parsePayload json
  msg :: IgoMsg <- decodeJson =<< stripType payload.content
  pure $ payload { content = msg }


defaultRequest :: RequestMatchPayload
defaultRequest =
  { terms: defaultTerms
  }

defaultOffer :: OfferMatchPayload
defaultOffer =
  { terms
  , myColor: Black
  , opponentKey: ""
  }
  where
    {terms} = defaultRequest

demoOfferPayload :: UserKey -> OfferMatchPayload
demoOfferPayload opponentKey =
  { myColor: Black
  , opponentKey
  , terms: defaultTerms
  }

defaultTerms :: GameTerms
defaultTerms =
  { size: 19
  , handicap: 0
  , komi: 5.5
  }
