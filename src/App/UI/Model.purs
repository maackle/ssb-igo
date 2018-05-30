module App.UI.Model where

import Prelude

import App.IgoMsg (AcceptMatchPayload, DeclineMatchPayload, GameTerms, IgoMove, MsgKey, OfferMatchPayload, PlayMovePayload, RequestMatchPayload, StoneColor(..), defaultTerms)
import App.IgoMsg as Msg
import DOM.Node.Types (Element)
import Data.Either (Either(..))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Ssb.Types (UserKey)

type Model =
  { flume :: FlumeState
  , whoami :: Maybe UserKey
  , devIdentity :: Maybe DevIdentity
  , userKeys :: StrMap User
  , userNames :: StrMap User
  , scratchOffer :: ScratchOffer
  , refs :: StrMap Element
  }

data DevIdentity
  = Alice | Bob | Charlie

derive instance genericDevIdentity :: Generic DevIdentity
derive instance eqDevIdentity :: Eq DevIdentity

type User =
  { key :: UserKey
  , name :: Maybe String
  }

type ScratchOffer =
  { terms :: GameTerms
  , myColor :: StoneColor
  , opponent :: Either String User
  , errorMsg :: Maybe String
  }

type EzModel =
  { db :: FlumeData
  , whoami :: UserKey
  }

ezify :: Model -> Maybe EzModel
ezify m =
  case m.flume, m.whoami of
    FlumeDb db, Just whoami -> Just {db, whoami}
    _, _ -> Nothing

data FlumeState
  = FlumeDb FlumeData
  | FlumeUnloaded
  | FlumeFailure String


type FlumeData =
  { offers :: StrMap IndexedOffer
  , declines :: StrMap IndexedDecline
  , requests :: StrMap IndexedRequest
  , matches :: StrMap IndexedMatch
  , moves :: StrMap IndexedMove
  }

initialDb :: FlumeData
initialDb =
  { offers: M.empty
  , declines: M.empty
  , requests: M.empty
  , matches: M.empty
  , moves: M.empty
  }

initialModel :: Model
initialModel =
  { flume: FlumeUnloaded
  , whoami: Nothing
  , devIdentity: Nothing
  , userKeys: M.empty
  , userNames: M.empty
  , scratchOffer:
    { terms: defaultTerms
    , myColor: Black
    , opponent: Left ""
    , errorMsg: Nothing
    }
  , refs: M.empty
  }

-- TODO: make newtype
data IndexedOffer = IndexedOffer OfferMatchPayload {author :: UserKey, key :: MsgKey}
data IndexedDecline = IndexedDecline DeclineMatchPayload {author :: UserKey, key :: MsgKey}
data IndexedRequest = IndexedRequest RequestMatchPayload {author :: UserKey, key :: MsgKey}
data IndexedMove = IndexedMove PlayMovePayload {rootAccept :: MsgKey} {author :: UserKey, key :: MsgKey}
newtype IndexedMatch = IndexedMatch
  { acceptPayload :: AcceptMatchPayload
  , offerPayload :: OfferMatchPayload
  , moves :: (Array MoveStep)
  , acceptMeta :: {author :: UserKey, key :: MsgKey}
  , offerMeta :: {author :: UserKey, key :: MsgKey}
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
