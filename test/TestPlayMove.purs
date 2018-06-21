module Test.TestPlayMove where

import Prelude

import App.Flume (IndexedMatch(..), MoveStep(..), isMatchEnd)
import App.IgoMsg (BoardPosition(..), IgoMove(..), IgoMsg(..), MsgKey, StoneColor(..))
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
import Ssb.Types (Sbot)
import Test.Common (FX, checkDb, createTestSbot, playMove, playMove', playbook2, sesh, setupMatch, tempConfig)
import Test.Spec (Group, describe, describeOnly, it, itOnly, pending, pending')
import Test.Spec.Assertions (shouldEqual)


testPlayMove :: (Config -> Eff FX Sbot) -> StateT (Array (Group (Aff FX Unit ) ) ) Identity Unit
testPlayMove startSbot = do
  let testbot = liftEff $ startSbot tempConfig
      boardPos = BoardPosition {x: 0, y: 0}
  describeOnly "PlayMove" do
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

    it "disallows move trees" $ sesh testbot \sbot -> do
      playbook2 sbot \alice bob -> do
        match <- setupMatch alice bob Black 0
        m1 <- playMove alice match
        m2 <- playMove bob match
        m3 <- playMove alice m2
        m4 <- playMove bob m1
        checkDb sbot \db -> do
          let
            moveKeys :: Maybe (Array MsgKey)
            moveKeys = M.lookup match db.matches <#> \(IndexedMatch {moves}) ->
                         moves <#> \(MoveStep {key}) -> key
          moveKeys `shouldEqual` Just [m1, m4]

    pending "sets subjectiveMoveNum"

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

    describe "game end state" do
      pending' "detects game end from passes" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          _ <- setupMatch alice bob Black 0
            >>= playMove alice
            >>= playMove bob
            >>= playMove alice
            >>= playMove bob
            >>= playMove' alice Pass
            >>= playMove' bob Pass
          pure unit

      it "lets anybody mark stones dead" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          _ <- setupMatch alice bob Black 0
            >>= playMove alice
            >>= playMove bob
            >>= playMove' alice Pass
            >>= playMove' bob Pass
            >>= playMove' alice Pass
            >>= playMove bob
            >>= playMove' alice Pass
            >>= playMove' bob Pass
            >>= playMove' bob (ToggleDead boardPos)
            >>= playMove' bob (ToggleDead boardPos)
            >>= playMove' alice (ToggleDead boardPos)
          checkDb sbot \db -> M.size db.moves `shouldEqual` 11

      it "prevents marking dead if game not ended" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          _ <- setupMatch alice bob Black 0
            >>= playMove' alice Pass
            >>= playMove' bob (ToggleDead boardPos)
          checkDb sbot \db -> M.size db.moves `shouldEqual` 1

      it "prevents finalization if game not ended" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          _ <- setupMatch alice bob Black 0
            >>= playMove' alice Pass
            >>= playMove' bob Finalize
          checkDb sbot \db -> M.size db.moves `shouldEqual` 1

      it "prevents double finalization" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          matchKey <- setupMatch alice bob Black 0
          _ <- pure matchKey
            >>= playMove' alice Pass
            >>= playMove' bob Pass
            >>= playMove' bob Finalize
            >>= playMove' bob Finalize
          checkDb sbot \db -> do
            M.size db.moves `shouldEqual` 3
            (isMatchEnd <$> M.lookup matchKey db.matches) `shouldEqual` (Just true)

      it "prevents all moves after finalization" $ sesh testbot \sbot ->
        playbook2 sbot \alice bob -> do
          finalKey <- setupMatch alice bob Black 0
            >>= playMove' alice Pass
            >>= playMove' bob Pass
            >>= playMove' bob Finalize
            >>= playMove' alice Finalize
          _ <- playMove' bob (PlayStone boardPos) finalKey
          _ <- playMove' bob (Pass) finalKey
          _ <- playMove' bob (Resign) finalKey
          _ <- playMove' bob (ToggleDead boardPos) finalKey
          checkDb sbot \db -> M.size db.moves `shouldEqual` 4

    describe "moveNumber" do
      pending "filters out game end moves"
