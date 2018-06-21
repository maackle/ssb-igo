module Test.Main where

import Prelude

import App.Flume (FlumeData, IndexedDecline(..), IndexedMatch(..), IndexedOffer(..), IndexedRequest(..), MoveStep(..), addUserKey)
import App.IgoMsg (BoardPosition(BoardPosition), IgoMove(PlayStone), IgoMsg(AcceptMatch, OfferMatch, WithdrawOffer, AcknowledgeDecline, DeclineMatch, ExpireRequest, RequestMatch, PlayMove), MsgKey, OfferMatchPayload, RequestMatchPayload, StoneColor(Black), SsbIgoMsg, publishMsg')
import App.UI.ClientQueries (getDb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Debug.Trace (traceA, traceAnyA)
import Ssb.Client (getClient, props)
import Ssb.Server (createFeed)
import Ssb.Types (UserKey, SbotConn)
import Test.Common (Conn, FX, checkDb, createTestSbot, normalConfig, playbook1, playbook2, playbook3, sesh, tempConfig)
import Test.Spec (describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Test.TestMatchSetup (testMatchSetup)
import Test.TestPlayMove (testPlayMove)
import Test.TestSsb (serverTests)



main :: Eff FX Unit
main = do
  startSbot <- createTestSbot
  let
    testbot = liftEff $ startSbot tempConfig
    fullbot = liftEff $ startSbot =<< normalConfig

    demoMsg :: IgoMsg
    demoMsg = RequestMatch
      { terms:
        { size: 19
        , handicap: 0
        , komi: 5.5
        }
      }

  run [consoleReporter] do
    serverTests startSbot
    testMatchSetup startSbot
    testPlayMove startSbot

    describe "client" do
      pending' "can connect" $ sesh fullbot \sbot -> do
        client <- getClient =<< liftEff normalConfig
        traceA "zee client"
        traceAnyA client
        shouldEqual 1 1

    describe "ssb-igo" do
      it "has a db" $ sesh testbot \sbot -> do
        checkDb sbot \db ->
          db.offers `shouldEqual` M.empty
