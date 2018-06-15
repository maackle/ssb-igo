module App.UI.Model where

import Prelude

import App.Flume (FlumeData, FlumeState(..))
import App.IgoMsg (GameTerms, OfferMatchPayload, StoneColor(Black), defaultTerms)
import App.UI.Routes (Route(..))
import DOM.Node.Types (Element)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Ssb.Types (UserKey)
import Tenuki.Game (TenukiClient, TenukiGame)

type Model =
  { flume :: FlumeState
  , whoami :: Maybe UserKey
  , devIdentity :: Maybe DevIdentity
  , userKeys :: StrMap User
  , userNames :: StrMap User
  , scratchOffer :: ScratchOffer
  , refs :: StrMap Element
  , route :: Route
  , tenukiClient :: Maybe TenukiClient
  , kibitzDraft :: String
  , showOfferForm :: Boolean
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
  , tenukiClient: Nothing
  , kibitzDraft: ""
  , showOfferForm: false
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
userNameFromKey {userKeys} key' =
  case M.lookup key' userKeys of
    Just {key, name} -> maybe key id name
    Nothing -> key'
