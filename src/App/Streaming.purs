module App.Streaming where

import Prelude

import App.Common (messageTypeString)
import App.IgoMsg (AcceptMatchPayload, BoardPosition(..), DeclineMatchPayload, IgoMsg(..), MsgKey, OfferMatchPayload, RequestMatchPayload, SsbMessage(SsbMessage), PlayMovePayload, parseMessage)
import App.Utils (trace')
import Data.Argonaut (Json, jsonNull, toObject, toString)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, delete, insert, lookup)
import Data.StrMap as M
import Data.Tuple.Nested ((/\))
import Partial (crashWith)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Ssb.Types (UserKey)


type FlumeDb =
  { offers :: StrMap IndexedOffer
  , declines :: StrMap IndexedDecline
  , requests :: StrMap IndexedRequest
  , matches :: StrMap IndexedMatch
  , moves :: StrMap IndexedMove
  }

initialDb :: FlumeDb
initialDb =
  { offers: M.empty
  , declines: M.empty
  , requests: M.empty
  , matches: M.empty
  , moves: M.empty
  }

-- TODO: make newtype
data IndexedOffer = IndexedOffer OfferMatchPayload {author :: UserKey}
data IndexedDecline = IndexedDecline DeclineMatchPayload {author :: UserKey}
data IndexedRequest = IndexedRequest RequestMatchPayload {author :: UserKey}
data IndexedMatch = IndexedMatch AcceptMatchPayload (Array MoveStep) {author :: UserKey}
data IndexedMove = IndexedMove PlayMovePayload {rootAccept :: MsgKey} {author :: UserKey}
newtype MoveStep = MoveStep {position :: BoardPosition, key :: MsgKey}

derive instance genericIndexedOffer :: Generic IndexedOffer
derive instance genericIndexedDecline :: Generic IndexedDecline
derive instance genericIndexedRequest :: Generic IndexedRequest
derive instance genericIndexedMatch :: Generic IndexedMatch
derive instance genericIndexedMove :: Generic IndexedMove
derive instance genericMoveStep :: Generic MoveStep

instance showIndexedOffer :: Show IndexedOffer where show = gShow
instance showIndexedDecline :: Show IndexedDecline where show = gShow
instance showIndexedRequest :: Show IndexedRequest where show = gShow
instance showIndexedMatch :: Show IndexedMatch where show = gShow
instance showIndexedMove :: Show IndexedMove where show = gShow

instance eqIndexedOffer :: Eq IndexedOffer where eq = gEq
instance eqIndexedDecline :: Eq IndexedDecline where eq = gEq
instance eqIndexedRequest :: Eq IndexedRequest where eq = gEq
instance eqIndexedMatch :: Eq IndexedMatch where eq = gEq
instance eqIndexedMove :: Eq IndexedMove where eq = gEq

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
      lookup targetKey db.requests
        # maybe db \(IndexedRequest _ meta) ->
          if author == meta.author
            then db { requests = delete targetKey db.requests }
            else db

    SsbMessage (OfferMatch payload) {key, author} ->
      db { offers = insert key (IndexedOffer payload {author}) db.offers }

    SsbMessage (WithdrawOffer targetKey) {author} ->
      lookup targetKey db.offers
        # maybe db \(IndexedOffer _ meta) ->
          if author == meta.author
            then db { offers = delete targetKey db.offers }
            else db

    SsbMessage (AcceptMatch payload@{offerKey}) {key, author} ->
      lookup offerKey db.offers
        # maybe db \(IndexedOffer {opponentKey} meta) ->
          if author == opponentKey
            then db { offers = delete offerKey db.offers
                    , matches = insert key (IndexedMatch payload [] {author}) db.matches
                    }
            else db

    SsbMessage (DeclineMatch payload@{offerKey}) {key, author} ->
      lookup offerKey db.offers
        # maybe db \(IndexedOffer {opponentKey} meta) ->
          if author == opponentKey
            then db { offers = delete offerKey db.offers
                    , declines = insert key (IndexedDecline payload {author}) db.declines
                    }
            else db

    SsbMessage (AcknowledgeDecline targetKey) {author} ->
      lookup targetKey db.declines
        # maybe db \(IndexedDecline {userKey} meta) ->
          if author == userKey
            then db { declines = delete targetKey db.declines }
            else db

    SsbMessage (PlayMove payload@{position, lastMove}) {author, key} ->
      let
        triple = do
          -- if no moves so far, then lastMove refers to the root Accept message
          -- else, get the rootAccept key from db.moves
          let rootAccept = M.lookup lastMove db.moves
                         # maybe lastMove \(IndexedMove _ {rootAccept} _) -> rootAccept
          IndexedMatch matchPayload matchMoves matchMeta <- M.lookup rootAccept db.matches
          let
            newMoves = snoc matchMoves $ MoveStep {position, key}
            newMatch = IndexedMatch matchPayload newMoves matchMeta
          pure
            $ rootAccept /\ IndexedMove payload {rootAccept} {author} /\ newMatch

      in case triple of
        Just (rootAccept /\ move /\ match@(IndexedMatch _ _ meta)) ->
          db { moves = M.insert key move db.moves
             , matches = M.insert rootAccept match db.matches }
        Nothing -> trace' ("move not found: " <> lastMove) db

    SsbMessage (Kibitz payload) _ ->
      trace' "TODO" db


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
