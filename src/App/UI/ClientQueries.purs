module App.UI.ClientQueries where

import Prelude

import App.Common (getClient')
import App.Streaming (decodeFlumeDb)
import App.UI.Model (FlumeData)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe')
import Debug.Trace (traceAnyA)
import Partial.Unsafe (unsafeCrashWith)
import Ssb.Client (ClientConnection)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)

getDb :: ∀ fx. ClientConnection -> Aff (ssb :: SSB | fx) FlumeData
getDb sbot = do
  json <- fromEffFnAff $ _getDb sbot
  case decodeFlumeDb json of
    Just db -> pure db
    Nothing -> pure $ unsafeCrashWith "can't decode raw db"

-- testFeed :: ∀ fx. String -> ClientConnection -> Aff (ssb :: SSB | fx) ClientConnection
-- testFeed path sbot = fromEffFnAff $ _testFeed sbot path

testFeeds =
  { alice: "./ssb-data-alice/secret"
  , bob: "./ssb-data-bob/secret"
  , charlie: "./ssb-data-charlie/secret"
  }

foreign import getStream :: ∀ fx. ClientConnection -> Eff (ssb :: SSB | fx) PullStream
foreign import _getDb :: ∀ fx. ClientConnection -> EffFnAff (ssb :: SSB | fx) Json
-- foreign import _testFeed :: ∀ fx. ClientConnection -> String -> EffFnAff (ssb :: SSB | fx) ClientConnection
