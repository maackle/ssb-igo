module Test.TestMatchSetup where

import Prelude

import App.Flume (IndexedDecline(..), IndexedOffer(..), IndexedRequest(..), addUserKey)
import App.IgoMsg (IgoMsg(..), OfferMatchPayload, RequestMatchPayload, StoneColor(..), MsgKey, publishMsg')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Ssb.Client (props)
import Ssb.Config (Config(..), SSB)
import Ssb.Friends (createFriendStream, hops)
import Ssb.Server (sbotBuilder)
import Ssb.Types (Sbot, UserKey)
import Test.Common (Conn, FX, checkDb, createTestSbot, offerPayload, playbook1, playbook2, playbook3, requestPayload, sesh, tempConfig)
import Test.Spec (Group, describe, it, itOnly, pending)
import Test.Spec.Assertions (shouldEqual)

testMatchSetup :: (Config -> Eff FX Sbot) -> StateT (Array (Group (Aff FX Unit ) ) ) Identity Unit
testMatchSetup startSbot = describe "Match setup" do
  let
    testbot = liftEff $ startSbot tempConfig

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
        _ <- publishMsg' charlie $ AcknowledgeDecline declineMeta.key
        _ <- publishMsg' bob $ AcknowledgeDecline declineMeta.key
        checkDb sbot \db -> do
          M.values db.declines `shouldEqual` [expected]
        _ <- publishMsg' alice $ AcknowledgeDecline declineMeta.key
        checkDb sbot \db -> do
          db.declines `shouldEqual` M.empty

  describe "WithdrawOffer" do
    it "lets an offer maker withdraw their own offer" $ sesh testbot \sbot -> do
      playbook2 sbot \alice bob -> do
        let offerData = offerPayload bob
        ({key}) <- publishMsg' alice $ OfferMatch offerData
        _ <- publishMsg' bob $ WithdrawOffer key
        checkDb sbot \db -> M.size db.offers `shouldEqual` 1
        _ <- publishMsg' alice $ WithdrawOffer key
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
