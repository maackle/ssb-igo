module App.Streaming where

import Prelude

import App.Common (messageTypeString)
import App.IgoMsg (IgoMsg(OfferMatch), MsgKey, OfferMatchRows, SsbMessage(SsbMessage), parseMessage)
import App.Utils (trace')
import Data.Argonaut (Json, jsonNull, toObject, toString)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe, maybe)
import Data.StrMap (lookup)
import Partial.Unsafe (unsafeCrashWith)


type FlumeDb =
  { --offers :: Map MsgKey OfferData
--  , requests :: Map MsgKey
  }

type ReduceFn = FlumeDb -> Json -> FlumeDb
type ReduceFnImpl = Fn2 FlumeDb Json FlumeDb
type MapFn = Json -> Json

data MessageType
  = ValidPayload Json
  | PrivateMessage
  | InvalidMessage

type OfferData = {msgKey :: MsgKey | OfferMatchRows}

reduceFn :: ReduceFn
reduceFn db json =
  case msg of
    msg@(SsbMessage (OfferMatch content) {key}) -> {}
      -- db { offers = insert key (content { msgKey = key }) db.offers }
    _ -> trace' "TODO :()" {}
  where
    msg = case parseMessage json of
      Right m -> m
      Left err -> unsafeCrashWith ("bad message: " <> err <> ". json = " <> show json)

mapFn :: MapFn
mapFn json = if isValidMessage json then json else trace' ("dropped message: " <> show json) jsonNull

isValidMessage :: Json -> Boolean
isValidMessage json = maybe false ((==) messageTypeString) (messageType json)

messageType :: Json -> Maybe String
messageType json = toObject json
  >>= lookup "value" >>= toObject
  >>= lookup "content" >>= toObject
  >>= lookup "type" >>= toString
