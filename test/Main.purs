module Test.Main where

import Prelude

import App.DB.Main (ssbIgoPlugin)
import App.IgoMsg (BoardPosition(BoardPosition), IgoMove(PlayStone), IgoMsg(AcceptMatch, OfferMatch, WithdrawOffer, AcknowledgeDecline, DeclineMatch, ExpireRequest, RequestMatch, PlayMove), MsgKey, OfferMatchPayload, RequestMatchPayload, StoneColor(Black), SsbIgoMsg, publishMsg')
import App.UI.ClientQueries (getDb)
import App.UI.Model (FlumeData, IndexedDecline(..), IndexedMatch(..), IndexedOffer(..), IndexedRequest(..), MoveStep(..), addUserKey)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Data.Traversable (sequence)
import Debug.Trace (traceA, traceAnyA)
import Ssb.Client (close, getClient, props, whoami)
import Ssb.Common (SA)
import Ssb.Config (Config(..), SSB, defaultConfigData)
import Ssb.Server (createFeed, requirePlugin, sbotBuilder, toPlugin)
import Ssb.Types (UserKey, SbotConn)
import Test.Common (Conn, FX, createTestSbot, normalConfig, sesh, tempConfig)
import Test.Spec (describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.TestSsb (serverTests)


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
      let
        move = PlayStone $ BoardPosition {x: 0, y: 0}
        payload = {move, lastMove, subjectiveMoveNum: -1}
      ({key}) <- publishMsg' agent $ PlayMove payload
      pure key

  run [consoleReporter] do
    serverTests startSbot

    describe "server" do
      it "can connect" $ sesh testbot \sbot -> do
        {id} <- whoami sbot
        let s = props sbot
        shouldEqual id s.id
      it "has feeds" $ sesh testbot \sbot -> do
        alice <- feed sbot
        bob <- feed sbot
        shouldNotEqual (alice # props # _.id) (bob # props # _.id)

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

      describe "RequestMatch" do
        it "indexes requests" $ sesh testbot \sbot -> do
          playbook1 sbot \alice -> do
            ({author, key}) <- publishMsg' alice $ RequestMatch requestPayload
            checkDb sbot \db -> do
              M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest requestPayload {author, key})
        pending "only allows one request per user"

      describe "ExpireRequest" do
        it "only cancels requests from original author" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            ({author, key}) <- publishMsg' alice $ RequestMatch requestPayload
            let expected = IndexedRequest requestPayload {author, key}
            checkDb sbot \db ->
              M.lookup key db.requests `shouldEqual` (Just $ IndexedRequest requestPayload {author, key})

            (meta2) <- publishMsg' bob $ ExpireRequest key
            checkDb sbot \db ->
              M.values db.requests `shouldEqual` [expected]

            (meta2) <- publishMsg' alice $ ExpireRequest key
            checkDb sbot \db ->
              db.requests `shouldEqual` M.empty

      describe "OfferMatch" do
        it "indexes offers" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            let payload = offerPayload bob
            ({author, key}) <- publishMsg' alice $ OfferMatch payload
            checkDb sbot \db ->
              M.lookup key db.offers `shouldEqual` (Just $ IndexedOffer payload {author, key})
        pending "disallows offer to self"

      describe "DeclineMatch" do
        it "lets target of OfferMatch decline" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            let offerData = offerPayload bob
            ({author, key}) <- publishMsg' alice $ OfferMatch offerData
            checkDb sbot \db ->
              M.lookup key db.offers `shouldEqual` (Just $ IndexedOffer offerData {author, key})

            let declinePayload = {offerKey: key, reason: Just "changed my mind"}
                declinePayload' = addUserKey author declinePayload
            (declineMeta) <- publishMsg' bob $ DeclineMatch declinePayload
            checkDb sbot \db -> do
              db.offers `shouldEqual` M.empty
              M.values db.declines `shouldEqual` [IndexedDecline declinePayload' {key: declineMeta.key, author: declineMeta.author}]

        it "prevents all others from declining" $ sesh testbot \sbot -> do
          playbook3 sbot \alice bob charlie -> do
            let offerData = offerPayload bob
            ({author, key}) <- publishMsg' alice $ OfferMatch offerData
            let expected = IndexedOffer offerData {author, key}
                declinePayload = {offerKey: key, reason: Just "changed my mind"}
                declinePayload' = addUserKey author declinePayload

            (declineMeta) <- publishMsg' alice $ DeclineMatch declinePayload
            checkDb sbot \db -> do
              M.values db.offers `shouldEqual` [expected]
              db.declines `shouldEqual` M.empty

            (declineMeta) <- publishMsg' charlie $ DeclineMatch declinePayload
            checkDb sbot \db -> do
              M.values db.offers `shouldEqual` [expected]
              db.declines `shouldEqual` M.empty

            (declineMeta) <- publishMsg' bob $ DeclineMatch declinePayload
            checkDb sbot \db -> do
              db.offers `shouldEqual` M.empty
              M.values db.declines `shouldEqual` [IndexedDecline declinePayload' {key: declineMeta.key, author: declineMeta.author}]

      describe "AcknowledgeDecline" do
        it "clears declines from index" $ sesh testbot \sbot -> do
          playbook3 sbot \alice bob charlie -> do
            let offerData = offerPayload bob
            (offerMeta) <- publishMsg' alice $ OfferMatch offerData
            let declinePayload =
                  { offerKey: offerMeta.key
                  , reason: Just "changed my mind" }
                declinePayload' = addUserKey offerMeta.author declinePayload

            (declineMeta) <- publishMsg' bob $ DeclineMatch declinePayload
            let expected = IndexedDecline declinePayload' {key: declineMeta.key, author: declineMeta.author}
            (_) <- publishMsg' charlie $ AcknowledgeDecline declineMeta.key
            (_) <- publishMsg' bob $ AcknowledgeDecline declineMeta.key
            checkDb sbot \db -> do
              M.values db.declines `shouldEqual` [expected]
            (_) <- publishMsg' alice $ AcknowledgeDecline declineMeta.key
            checkDb sbot \db -> do
              db.declines `shouldEqual` M.empty

      describe "WithdrawOffer" do
        it "lets an offer maker withdraw their own offer" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            let offerData = offerPayload bob
            ({key}) <- publishMsg' alice $ OfferMatch offerData
            (_) <- publishMsg' bob $ WithdrawOffer key
            checkDb sbot \db -> M.size db.offers `shouldEqual` 1
            (_) <- publishMsg' alice $ WithdrawOffer key
            checkDb sbot \db -> M.size db.offers `shouldEqual` 0

      describe "AcceptMatch" do
        it "lets only intended opponent accept" $ sesh testbot \sbot -> do
          playbook3 sbot \alice bob charlie -> do
            let offerData@{terms} = offerPayload bob
            ({key}) <- publishMsg' alice $ OfferMatch offerData
            _ <- publishMsg' charlie $ AcceptMatch {offerKey: key, terms}
            _ <- publishMsg' alice $ AcceptMatch {offerKey: key, terms}
            checkDb sbot \db -> M.size db.matches `shouldEqual` 0
            _ <- publishMsg' bob $ AcceptMatch {offerKey: key, terms}
            checkDb sbot \db -> do
              M.size db.matches `shouldEqual` 1

      describe "PlayMove" do
        it "indexes moves" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            match <- setupMatch alice bob Black 0
            checkDb sbot \db -> do
              M.size db.matches `shouldEqual` 1
            m1 <- playMove alice match
            m2 <- playMove bob m1
            _ <- playMove alice m2
            checkDb sbot \db -> do
              M.size db.moves `shouldEqual` 3

        it "updates match index" $ sesh testbot \sbot -> do
          playbook2 sbot \alice bob -> do
            match <- setupMatch alice bob Black 0
            m1 <- playMove alice match
            m2 <- playMove bob m1
            m3 <- playMove bob m2
            checkDb sbot \db -> do
              let
                moveKeys :: Maybe (Array MsgKey)
                moveKeys = M.lookup match db.matches <#> \(IndexedMatch {moves}) ->
                            moves <#> \(MoveStep {key}) -> key
              moveKeys `shouldEqual` Just [m1, m2]

        describe "first move detection" do
          it "does even games" $ sesh testbot \sbot ->
            playbook2 sbot \alice bob -> do
              match <- setupMatch alice bob Black 0
              _ <- playMove bob match
              checkDb sbot \db -> M.size db.moves `shouldEqual` 0
              _ <- playMove alice match
              checkDb sbot \db -> M.size db.moves `shouldEqual` 1
          it "does handicap games" $ sesh testbot \sbot ->
            playbook2 sbot \alice bob -> do
              match <- setupMatch alice bob Black 3
              _ <- playMove alice match
              checkDb sbot \db -> M.size db.moves `shouldEqual` 0
              _ <- playMove bob match
              checkDb sbot \db -> M.size db.moves `shouldEqual` 1

            -- let offerData@{terms} = offerPayload bob
            -- ({key}) <- publishMsg' alice $ OfferMatch offerData
            -- _ <- publishMsg' bob $ AcceptMatch {offerKey: key, terms}
