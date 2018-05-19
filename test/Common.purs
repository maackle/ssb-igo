module Test.Common where

import Prelude

import App.DB.Main (ssbIgoPlugin)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Ssb.Client (close)
import Ssb.Config (Config(..), SSB, defaultConfigData)
import Ssb.Server (requirePlugin, sbotBuilder, toPlugin)
import Ssb.Types (SbotConn)
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
