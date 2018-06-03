module App.UI.Model where

import Prelude

import App.IgoMsg (AcceptMatchPayload, DeclineMatchPayload, GameTerms, IgoMove, MsgKey, OfferMatchPayload, PlayMovePayload, RequestMatchPayload, StoneColor(..), DeclineMatchFields, defaultTerms)
import App.IgoMsg as Msg
import App.UI.Routes (Route(..))
import DOM.Node.Types (Element)
import Data.Either (Either(..), either)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Record as Record
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import Ssb.Types (UserKey)
import Tenuki.Game (TenukiGame)

type Model =
  { flume :: FlumeState
  , whoami :: Maybe UserKey
  , devIdentity :: Maybe DevIdentity
  , userKeys :: StrMap User
  , userNames :: StrMap User
  , scratchOffer :: ScratchOffer
  , refs :: StrMap Element
  , route :: Route
  , tenukiGame :: Maybe TenukiGame
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
  , myName :: String
  }

scratchOfferToOfferPayload :: ScratchOffer -> Maybe OfferMatchPayload
scratchOfferToOfferPayload {terms, myColor, opponent} = case opponent of
  Left _ -> Nothing
  Right {key} -> Just {terms, myColor, opponentKey: key}

ezify :: Model -> Maybe EzModel
ezify m =
  case m.flume, m.whoami of
    FlumeDb db, Just whoami ->
      let
        myName = maybe whoami id do
          user <- M.lookup whoami m.userKeys
          user.name
      in Just
          { db
          , whoami
          , myName
          }
    _, _ -> Nothing

userNameFromKey :: Model -> UserKey -> String
userNameFromKey {userKeys} key =
  case M.lookup key userKeys of
    Just {key, name} -> maybe key id name
    Nothing -> key

assignColors :: IndexedOffer -> {black :: UserKey, white :: UserKey}
assignColors (IndexedOffer payload meta) = assignColors' payload meta

assignColors' :: ∀ a. OfferMatchPayload -> { author :: UserKey | a} -> {black :: UserKey, white :: UserKey}
assignColors' {myColor, opponentKey} {author} =
  case myColor of
    Black -> { black: author, white: opponentKey}
    White -> { white: author, black: opponentKey}

addUserKey :: ∀ a. String -> DeclineMatchPayload -> {userKey :: String | DeclineMatchFields}
addUserKey = Record.insert (SProxy :: SProxy "userKey")

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
  , devIdentity: Just Alice
  , userKeys: M.empty
  , userNames: M.empty
  , scratchOffer:
    { terms: defaultTerms
    , myColor: Black
    , opponent: Left ""
    , errorMsg: Nothing
    }
  , refs: M.empty
  , route: Dashboard
  , tenukiGame: Nothing
  }

-- TODO: make newtype
data IndexedOffer = IndexedOffer OfferMatchPayload {author :: UserKey, key :: MsgKey}
data IndexedDecline = IndexedDecline {userKey :: UserKey | DeclineMatchFields} {author :: UserKey, key :: MsgKey}
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

derive instance newtypeMoveStep :: Newtype MoveStep _

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
