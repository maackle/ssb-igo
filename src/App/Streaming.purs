module App.Streaming where

import Prelude

import App.Common (messageTypeString)
import App.IgoMsg (AcceptMatchPayload, BoardPosition(..), DeclineMatchPayload, IgoMove(..), IgoMsg(..), MsgKey, OfferMatchPayload, PlayMovePayload, RequestMatchPayload, SsbMessage(SsbMessage), StoneColor(..), parseMessage)
import App.Utils (trace', (&))
import Data.Argonaut (Json, jsonNull, toObject, toString)
import Data.Array (length, snoc)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.StrMap (StrMap, delete, insert, lookup)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy, trace, traceAny)
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
data IndexedMove = IndexedMove PlayMovePayload {rootAccept :: MsgKey} {author :: UserKey}
newtype IndexedMatch = IndexedMatch
  { acceptPayload :: AcceptMatchPayload
  , offerPayload :: OfferMatchPayload
  , moves :: (Array MoveStep)
  , acceptMeta :: {author :: UserKey}
  , offerMeta :: {author :: UserKey}
  }
derive instance newtypeIndexedMatch :: Newtype IndexedMatch _

newtype MoveStep = MoveStep {move :: IgoMove, key :: MsgKey}
type OpponentKey = UserKey

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

    SsbMessage (AcceptMatch acceptPayload@{offerKey}) acceptMeta@{key} ->
      lookup offerKey db.offers
        # maybe db \(IndexedOffer offerPayload@{opponentKey} offerMeta) ->
          if acceptMeta.author == opponentKey
            then
              let match = IndexedMatch
                            { acceptPayload
                            , offerPayload
                            , moves: []
                            , acceptMeta: {author: acceptMeta.author}
                            , offerMeta: {author: offerMeta.author}
                            }
              in db { offers = delete offerKey db.offers
                    , matches = insert key match db.matches
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

    SsbMessage (PlayMove payload@{move, lastMove}) {author, key} ->
      let
        -- there was a last move?
        maybePrev = M.lookup lastMove db.moves
        -- ascertain root accept key
        rootAccept = maybePrev # maybe lastMove \(IndexedMove _ d _) -> d.rootAccept
        -- look up the match based on root accept
        _ = traceAny maybePrev
        _ = traceAny rootAccept
        match = case M.lookup rootAccept db.matches of
          Just m -> m
          Nothing -> unsafeCrashWith $ "can't find match: " <> rootAccept

        moveError = validateMove match payload author
      in
        case moveError of
          Nothing ->
            let
              newMove = IndexedMove payload {rootAccept} {author}
              moveStep = MoveStep {move, key}
              newMatch = match # unwrap >>> (\m -> m { moves = snoc m.moves moveStep }) >>> wrap
            in db { moves   = M.insert key newMove db.moves
                  , matches = M.insert rootAccept newMatch db.matches}
          Just err ->
            trace ("move validation error: " <> err) $ const db

      --
      --
      --   newMatch = maybeMatch <#> unwrap >>> \d -> d { moves = snoc d.moves (MoveStep {move, key}) } >>> wrap
      --
      --   triple = do
      --     -- if no moves so far, then lastMove refers to the root Accept message
      --     -- else, get the rootAccept key from db.moves
      --     let rootAccept = M.lookup lastMove db.moves
      --                    # maybe lastMove \(IndexedMove _ {rootAccept} _) -> rootAccept
      --     IndexedMatch matchData <- M.lookup rootAccept db.matches
      --
      --     let
      --       newMoves = snoc matchData.moves $ MoveStep {move, key}
      --       newMatch = IndexedMatch $ matchData { moves = newMoves }
      --     pure
      --       $ rootAccept & IndexedMove payload {rootAccept} {author} & newMatch
      --
      -- in case triple of
      --   Just (rootAccept /\ move /\ match@(IndexedMatch _)) ->
      --     db { moves   = M.insert key move db.moves
      --        , matches = M.insert rootAccept match db.matches
      --        }
      --   Nothing -> trace' ("move not found: " <> lastMove) db
      --




    SsbMessage (Kibitz payload) _ ->
      trace' "TODO" db

  where

    nextMover :: FlumeDb -> IndexedMatch -> PlayMovePayload -> UserKey
    nextMover db (IndexedMatch {offerPayload, offerMeta, moves}) movePayload@{lastMove} =
      method2
      where
        {author} = offerMeta
        {terms, myColor, opponentKey} = offerPayload
        {handicap} = terms
        firstMover = if (myColor == Black) == (handicap == 0)
                        then author
                        else opponentKey
        method2 = case M.lookup lastMove db.moves of
                    Just (IndexedMove _ _ lastMeta) -> if author == lastMeta.author then opponentKey else author
                    Nothing -> firstMover

    validateMove :: IndexedMatch -> PlayMovePayload -> UserKey -> Maybe String
    validateMove match@(IndexedMatch {offerPayload, offerMeta, moves}) movePayload@{move, lastMove} author =
      case move of
        PlayStone position ->
          if author == nextMover db match movePayload then Nothing else Just $ "not your turn to move! " <> author
        Pass ->
          if author == nextMover db match movePayload then Nothing else Just "not your turn to pass!"
        Resign ->
          Nothing
      where
        {terms, myColor} = offerPayload
        {handicap} = terms
        {black, white} = getPlayers match

    getPlayers :: IndexedMatch -> {black :: UserKey, white :: UserKey}
    getPlayers (IndexedMatch {offerPayload, offerMeta}) =
      case myColor of
        Black -> { black: author, white: opponentKey}
        White -> { white: author, black: opponentKey}
      where
        {author} = offerMeta
        {terms, myColor, opponentKey} = offerPayload
        {handicap} = terms


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
