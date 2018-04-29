module App.IgoMsg where

import Prelude

import App.Utils (toObject')
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, info)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, fromObject, fromString, toObject)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Ssb.Client (getClient, publish)
import Ssb.Config (SSB, Config, defaultConfig)

type SF eff a = Aff (ssb :: SSB, console :: CONSOLE | eff) a

data IgoMsg
  = RequestMatch GameTerms
  | ExpireRequest
  | OfferMatch {terms :: GameTerms, userKey :: UserKey, opponentColor :: StoneColor}
  | WithdrawOffer MessageKey
  | AcceptMatch MessageKey
  | DeclineMatch MessageKey
  | PlayMove { position :: BoardPosition, lastMove :: MessageKey, subjectiveMoveNum :: Int }
  | Kibitz { move :: MessageKey, text :: String }

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

clientConfig = liftEff $ do
  cfg <- defaultConfig $ Just
            { path: "/Users/michael/.ssb-test"
            , keys: Nothing }
  pure cfg { port = 8088, shs = "GVZDyNf1TrZuGv3W5Dpef0vaITW1UqOUO3aWLNBp+7A=" }

publishMsg :: ∀ eff. IgoMsg -> SF eff Unit
publishMsg msg = do
  client <- getClient =<< clientConfig
  info "publishing"
  _ <- publish client $ toJson msg
  info "donezo"
  pure unit

toJson :: IgoMsg -> Json
toJson msg = case toObject' $ encodeJson msg of
  Right o -> fromObject $ insert "type" (fromString "igo") o
  Left err -> unsafePartial $ crashWith err


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
