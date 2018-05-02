module App.Streaming where

import Prelude

import App.Common (messageTypeString)
import App.IgoMsg (AcceptMatchPayload, DeclineMatchPayload, IgoMsg(..), MsgKey, OfferMatchPayload, RequestMatchPayload, SsbMessage(SsbMessage), parseMessage)
import App.Utils (trace')
import Data.Argonaut (Json, jsonNull, toObject, toString)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, delete, insert, lookup)
import Partial.Unsafe (unsafeCrashWith)
import Ssb.Types (UserKey)


type FlumeDb =
  { offers :: StrMap IndexedOffer
  , declines :: StrMap IndexedDecline
  , requests :: StrMap IndexedRequest
  , matches :: StrMap IndexedAccept
  }

-- TODO: make newtype
data IndexedOffer = IndexedOffer OfferMatchPayload {author :: UserKey}
data IndexedDecline = IndexedDecline DeclineMatchPayload {opponentKey :: UserKey}
data IndexedRequest = IndexedRequest RequestMatchPayload {author :: UserKey}
data IndexedAccept = IndexedAccept AcceptMatchPayload {author :: UserKey}

type ReduceFn = FlumeDb -> Json -> FlumeDb
type ReduceFnImpl = Fn2 FlumeDb Json FlumeDb
type MapFn = Json -> Json

data MessageType
  = ValidPayload Json
  | PrivateMessage
  | InvalidMessage


reduceFn :: ReduceFn
reduceFn db json =
  case msg of
    SsbMessage (RequestMatch payload) {key, author} ->
      db { requests = insert key (IndexedRequest payload {author}) db.requests }

    SsbMessage (ExpireRequest targetKey) {author} ->
      case lookup targetKey db.requests of
        Nothing -> db
        Just (IndexedRequest _ meta) ->
          if author == meta.author
            then db { requests = delete targetKey db.requests }
            else db

    SsbMessage (OfferMatch payload) {key, author} ->
      db { offers = insert key (IndexedOffer payload {author}) db.offers }

    SsbMessage (DeclineMatch payload@{offerKey}) {key, author} ->
      case lookup offerKey db.offers of
        Nothing -> db
        Just (IndexedOffer {opponentKey} meta) ->
          if author == opponentKey
            then db { offers = delete offerKey db.offers
                    , declines = insert key (IndexedDecline payload {opponentKey}) db.declines
                    }
            else db

    SsbMessage (AcknowledgeDecline targetKey) {author} ->
      case lookup targetKey db.declines of
        Nothing -> db
        Just (IndexedDecline _ {opponentKey}) ->
          if author == opponentKey
            then db { declines = delete targetKey db.declines }
            else db

    _ -> trace' "TODO :()" db
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
