module Test.Common where

import Prelude

import App.DB.Main (ssbIgoPlugin)
import App.Flume (FlumeData)
import App.IgoMsg (BoardPosition(..), IgoMove(..), IgoMsg(..), MsgKey, OfferMatchPayload, RequestMatchPayload, SsbIgoMsg, StoneColor(..), publishMsg')
import App.UI.ClientQueries (getDb)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Ssb.Client (close, props)
import Ssb.Common (SA)
import Ssb.Config (Config(..), SSB, defaultConfigData)
import Ssb.Server (createFeed, requirePlugin, sbotBuilder, toPlugin)
import Ssb.Types (SbotConn, UserKey)
import Test.Spec.Runner (RunnerEffects)


tempConfig = TestConfig { port: 7531, host: "0.0.0.0" }
normalConfig = do
  cfg <- defaultConfigData $ Just
            { path: "./ssb-test-data"
            , keys: Nothing }
  pure $ Config $ cfg { port = 98765 }

createTestSbot = do
  plugins <- sequence
    [ requirePlugin "scuttlebot/plugins/replicate"
    , requirePlugin "ssb-private"
    , requirePlugin "ssb-friends"
    , pure $ toPlugin ssbIgoPlugin]
  sbotBuilder plugins


type FX = (RunnerEffects (ssb :: SSB, console :: CONSOLE, exception :: EXCEPTION))
type Conn = SbotConn

sesh :: (Aff FX Conn) -> (Conn -> Aff FX Unit) -> Aff FX Unit
sesh create runTest = do
  sbot <- create
  result <- attempt $ runTest sbot
  close sbot
  either (liftEff <<< throwException) pure result


feed = liftEff <<< createFeed

playbook1 :: ∀ fx. Conn -> (Conn -> SA fx Unit) -> SA fx Unit
playbook1 sbot play = do
  a <- feed sbot
  play a

playbook2 :: ∀ fx. Conn -> (Conn -> Conn -> SA fx Unit) -> SA fx Unit
playbook2 sbot play = do
  a <- feed sbot
  b <- feed sbot
  play a b

playbook3 :: ∀ fx. Conn -> (Conn -> Conn -> Conn -> SA fx Unit) -> SA fx Unit
playbook3 sbot play = do
  a <- feed sbot
  b <- feed sbot
  c <- feed sbot
  play a b c

checkDb :: SbotConn -> (FlumeData -> Aff FX Unit) -> Aff FX Unit
checkDb sbot check = do
  db <- getDb sbot
  check db

pubAndCheckDb :: SbotConn -> IgoMsg -> (SsbIgoMsg -> FlumeData -> Aff FX Unit) -> Aff FX Unit
pubAndCheckDb sbot m check = do
  msg <- publishMsg' sbot m
  db <- getDb sbot
  check msg db

----------------------------------------------------
----------------------------------------------------

testTerms =
  { size: 19
  , handicap: 0
  , komi: 5.5
  }

requestPayload :: RequestMatchPayload
requestPayload =
  { terms: testTerms
  }

offerPayload :: Conn -> OfferMatchPayload
offerPayload = offerPayload' <<< _.id <<< props

offerPayload' :: UserKey -> OfferMatchPayload
offerPayload' opponentKey =
  { myColor: Black
  , opponentKey
  , terms: testTerms
  }

setupMatch :: Conn -> Conn -> StoneColor -> Int -> Aff FX MsgKey
setupMatch alice bob myColor handicap = do
  let
    terms = {size: 19, komi: 0.5, handicap}
    opponentKey = props bob # _.id
    offerData = {myColor, opponentKey, terms}
  (offerMeta) <- publishMsg' alice $ OfferMatch offerData
  (acceptMeta) <- publishMsg' bob $ AcceptMatch {offerKey: offerMeta.key, terms}
  pure acceptMeta.key


playMove :: Conn -> MsgKey -> Aff FX MsgKey
playMove agent lastMove = do
  let move = PlayStone $ BoardPosition {x: 0, y: 0}
  playMove' agent move lastMove

playMove' :: Conn -> IgoMove -> MsgKey -> Aff FX MsgKey
playMove' agent move lastMove = do
  let
    payload = {move, lastMove, subjectiveMoveNum: -1}
  ({key}) <- publishMsg' agent $ PlayMove payload
  pure key
