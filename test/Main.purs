module Test.Main where

import Prelude

import App.DB.Main (ssbIgoPlugin)
import App.IgoMsg (IgoMsg(..), OfferMatchPayload, RequestMatchPayload, SsbMessage(..), StoneColor(..), publishMsg')
import App.Streaming (FlumeDb, IndexedDecline(..), IndexedOffer(..), IndexedRequest(..))
import App.UI.ClientQueries (getDb)
import App.Utils ((!))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (NonEmptyArray, replicate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromRight)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug.Trace (traceA, traceAnyA)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Ssb.Client (ClientConnection, close, getClient, props, whoami)
import Ssb.Common (SA)
import Ssb.Config (Config(..), SSB, defaultConfigData)
import Ssb.Server (Plugin, createFeed, loadPlugins, requirePlugin, sbotBuilder, toPlugin)
import Ssb.Types (UserKey)
import Test.Spec (describe, describeOnly, it, itOnly, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

tempConfig = TestConfig { port: 5432, host: "0.0.0.0" }
normalConfig = do
  cfg <- defaultConfigData $ Just
            { path: "./ssb-test-data"
            , keys: Nothing }
  pure $ Config $ cfg { port = 98765 }

type FX = (RunnerEffects (ssb :: SSB, console :: CONSOLE, exception :: EXCEPTION))
type Conn = ClientConnection

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

withSession :: (Aff FX Conn) -> (Conn -> Aff FX Unit) -> Aff FX Unit
withSession create runTest = do
  sbot <- create
  result <- attempt $ runTest sbot
  close sbot
  either (liftEff <<< throwException) pure result

checkDb :: ClientConnection -> (FlumeDb -> Aff FX Unit) -> Aff FX Unit
checkDb sbot check = do
  db <- getDb sbot
  check db

pubAndCheckDb :: ClientConnection -> IgoMsg -> (SsbMessage -> FlumeDb -> Aff FX Unit) -> Aff FX Unit
pubAndCheckDb sbot m check = do
  msg <- publishMsg' sbot m
  db <- getDb sbot
  check msg db

main :: Eff FX Unit
main = do
  traceA "loading"
  plugins <- sequence [requirePlugin "ssb-private", pure $ toPlugin ssbIgoPlugin]
  startSbot <- sbotBuilder plugins
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

    testTerms =
      { size: 19
      , handicap: 0
      , komi: 5.5
      }

    requestPayload :: RequestMatchPayload
    requestPayload =
      { terms: testTerms
      }

    offerPayload :: UserKey -> OfferMatchPayload
    offerPayload opponentKey =
      { myColor: Black
      , opponentKey
      , terms: testTerms
      }


  run [consoleReporter] do
    describe "server" do
      it "can connect" $ withSession testbot \sbot -> do
        {id} <- whoami sbot
        let s = props sbot
        shouldEqual id s.id
      it "has feeds" $ withSession testbot \sbot -> do
        alice <- feed sbot
        bob <- feed sbot
        shouldNotEqual (alice # props # _.id) (bob # props # _.id)

    describe "client" do
      pending' "can connect" $ withSession fullbot \sbot -> do
        client <- getClient =<< liftEff normalConfig
        traceA "zee client"
        traceAnyA client
        shouldEqual 1 1

    describe "ssb-igo" do
      it "has a db" $ withSession testbot \sbot -> do
        db <- getDb sbot
        db.offers `shouldEqual` M.empty

      describe "RequestMatch" do
        it "indexes requests" $ withSession testbot \sbot -> do
          playbook1 sbot \alice -> do
            (SsbMessage _ {author, key}) <- publishMsg' alice $ RequestMatch requestPayload
            db <- getDb sbot
            M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest requestPayload {author})

      describe "ExpireRequest" do
        it "only cancels requests from original author" $ withSession testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            (SsbMessage _ {author, key}) <- publishMsg' alice $ RequestMatch requestPayload
            let expected = IndexedRequest requestPayload {author}
            db <- getDb sbot
            M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest requestPayload {author})

            (SsbMessage _ meta2) <- publishMsg' bob $ ExpireRequest key
            db <- getDb sbot
            M.values db.requests `shouldEqual` [expected]

            (SsbMessage _ meta2) <- publishMsg' alice $ ExpireRequest key
            db <- getDb sbot
            db.requests `shouldEqual` M.empty

      describe "OfferMatch" do
        it "indexes offers" $ withSession testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            let payload = offerPayload $ props bob # _.id
            (SsbMessage _ {author, key}) <- publishMsg' alice $ OfferMatch payload
            checkDb sbot \db ->
              M.lookup key db.offers `shouldEqual` (Just $ IndexedOffer payload {author})

      describe "DeclineMatch" do
        it "lets target of OfferMatch decline" $ withSession testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            let offerData = offerPayload $ props bob # _.id
            (SsbMessage _ {author, key}) <- publishMsg' alice $ OfferMatch offerData
            checkDb sbot \db ->
              M.lookup key db.offers `shouldEqual` (Just $ IndexedOffer offerData {author})

            let declinePayload = {offerKey: key, userKey: author, reason: Just "changed my mind"}
            (SsbMessage _ declineMeta) <- publishMsg' bob $ DeclineMatch declinePayload
            checkDb sbot \db -> do
              M.lookup declineMeta.key db.offers `shouldEqual` Nothing
              M.values db.declines `shouldEqual` [IndexedDecline declinePayload {author: declineMeta.author}]
