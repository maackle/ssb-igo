module Test.Main where

import Prelude

import App.DB.Main (ssbIgoPlugin)
import App.IgoMsg (IgoMsg(..), SsbMessage(..), demoTerms, publishMsg')
import App.Streaming (IndexedRequest(..))
import App.UI.ClientQueries (getDb)
import App.Utils ((!))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Trans.Class (lift)
import Data.Array.NonEmpty (NonEmptyArray, replicate)
import Data.Either (Either(..), fromRight)
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

type FX = (RunnerEffects (ssb :: SSB))
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

withbot :: (Aff FX Conn) -> (Conn -> Aff FX Unit) -> Aff FX Unit
withbot create runTest = do
  sbot <- create
  runTest sbot
  close sbot

main :: Eff FX Unit
main = do
  traceA "loading"
  plugins <- sequence [requirePlugin "ssb-private", pure $ toPlugin ssbIgoPlugin]
  startSbot <- sbotBuilder plugins
  let
    testbot = liftEff $ startSbot tempConfig
    fullbot = liftEff $ startSbot =<< normalConfig

  run [consoleReporter] do
    describe "server" do
      it "can connect" $ withbot testbot \sbot -> do
        {id} <- whoami sbot
        let s = props sbot
        shouldEqual id s.id
      it "has feeds" $ withbot testbot \sbot -> do
        alice <- feed sbot
        bob <- feed sbot
        shouldNotEqual (alice # props # _.id) (bob # props # _.id)

    describe "client" do
      pending' "can connect" $ withbot fullbot \sbot -> do
        client <- getClient =<< liftEff normalConfig
        traceA "zee client"
        traceAnyA client
        shouldEqual 1 1

    describe "plugin" do
      it "has a db" $ withbot testbot \sbot -> do
        db <- getDb sbot
        db.offers `shouldEqual` M.empty
      it "tracks offers" $ withbot testbot \sbot -> do
        playbook1 sbot \alice -> do
          let payload = {terms: demoTerms}
          (SsbMessage _ {author, key}) <- publishMsg' alice $ RequestMatch payload
          db <- getDb sbot
          M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest payload {author})
      it "cancels offers" $ withbot testbot \sbot -> do
        playbook1 sbot \alice -> do
          let payload = {terms: demoTerms}
          (SsbMessage _ {author, key}) <- publishMsg' alice $ RequestMatch payload
          db <- getDb sbot
          M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest payload {author})
          (SsbMessage _ meta2) <- publishMsg' alice $ ExpireRequest key
          db.requests `shouldEqual` M.empty
