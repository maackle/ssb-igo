module App.IgoMsg where

import Prelude

import App.Common (getClient')
import App.Utils (lookup', lookup_, maybeToEither, toObject')
import Control.Monad.Aff (Aff)
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
import Ssb.Client (getClient, publish, close)
import Ssb.Config (SSB, Config, defaultConfig)

type SF eff a = Aff (ssb :: SSB | eff) a

data IgoMsg
  = RequestMatch GameTerms
  | ExpireRequest
  | OfferMatch {terms :: GameTerms, userKey :: UserKey, opponentColor :: StoneColor}
  | WithdrawOffer MessageKey
  | AcceptMatch MessageKey
  | DeclineMatch MessageKey
  | PlayMove { position :: BoardPosition, lastMove :: MessageKey, subjectiveMoveNum :: Int }
  | Kibitz { move :: MessageKey, text :: String }

data SsbMessage = SsbMessage IgoMsg
  { key :: String
  , previous :: String
  , author :: String
  , timestamp :: Number  -- inner timestamp
  , hash :: String
  , signature :: String
  }

type MessageKey = String

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

publishMsg :: ∀ eff. IgoMsg -> SF eff Unit
publishMsg msg = do
  client <- getClient'
  _ <- publish client $ toJson msg
  pure unit

toJson :: IgoMsg -> Json
toJson msg = case toObject' $ encodeJson msg of
  Right o -> fromObject $ M.insert "type" (fromString "igo") o
  Left err -> unsafePartial $ crashWith err

stripType :: Json -> Either String Json
stripType json = do
  o :: JObject <- toObject' json
  msgType :: String <- maybeToEither "no type specified" $ toString =<< M.lookup "type" o
  if msgType == "igo"
    then Right $ fromObject $ M.delete "type" o
    else Left "not an ssb-igo message"

parseMessage :: Json -> Either String SsbMessage
parseMessage json = do
  o <- toObject' json
  key <- lookup' "key" toString o
  value <- lookup' "value" toObject o

  previous <- lookup' "previous" toString value
  author <- lookup' "author" toString value
  timestamp <- lookup' "timestamp" (toNumber) value
  hash <- lookup' "hash" toString value
  signature <- lookup' "signature" toString value
  content <- lookup_ "content" value

  msg <- decodeJson =<< stripType content
  pure $ SsbMessage msg {key, previous, author, timestamp, hash, signature}


demoMsg :: IgoMsg
demoMsg = OfferMatch
  { userKey: "a"
  , opponentColor: Black
  , terms:
    { size: 19
    , handicap: 0
    , komi: 5.5
    }
  }
