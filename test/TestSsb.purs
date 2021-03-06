module Test.TestSsb where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT(..))
import Data.Identity (Identity(..))
import Data.StrMap as M
import Ssb.Client (props)
import Ssb.Config (Config(..), SSB)
import Ssb.Friends (createFriendStream, hops)
import Ssb.Server (sbotBuilder)
import Ssb.Types (Sbot)
import Test.Common (FX, createTestSbot, sesh, tempConfig)
import Test.Spec (Group, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)

-- testbot :: ∀ fx. Aff (ssb :: SSB | fx) Sbot
-- testbot = liftEff do
--   create <- createTestSbot
--   create tempConfig

serverTests :: (Config -> Eff FX Sbot) -> StateT (Array (Group (Aff FX Unit ) ) ) Identity Unit
serverTests startSbot = do
  let testbot = liftEff $ startSbot tempConfig
  describe "Ssb" do
    describe "Friends" do
      it "implements createFriendStream" $ sesh testbot \sbot -> do
        stream <- liftEff $ createFriendStream sbot {}
        shouldEqual 1 1
      it "implements hops" $ sesh testbot \sbot -> do
        friends <- hops sbot (props sbot # _.id)
        M.size friends `shouldEqual` 0
