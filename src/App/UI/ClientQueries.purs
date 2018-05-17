module App.UI.ClientQueries where

import Prelude

import App.Common (devConfig, getClient')
import App.Streaming (decodeFlumeDb)
import App.UI.Model (DevIdentity(..), FlumeData)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe')
import Debug.Trace (traceAnyA)
import Partial.Unsafe (unsafeCrashWith)
import Ssb.Client (ClientConnection, getClient)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)

getDb :: ∀ fx. ClientConnection -> Aff (ssb :: SSB | fx) FlumeData
getDb sbot = do
  json <- fromEffFnAff $ _getDb sbot
  case decodeFlumeDb json of
    Just db -> pure db
    Nothing -> pure $ unsafeCrashWith "can't decode raw db"

devClient :: ∀ fx. DevIdentity -> Aff (ssb :: SSB | fx) ClientConnection
devClient = case _ of
  Alice -> tc "alice" 8081
  Bob -> tc "bob" 8082
  Charlie -> tc "charlie" 8083
  where
    tc name port =
      getClient =<< (liftEff $ devConfig ("./ssb-dev-" <> name) port)

foreign import getStream :: ∀ fx. ClientConnection -> Eff (ssb :: SSB | fx) PullStream
foreign import _getDb :: ∀ fx. ClientConnection -> EffFnAff (ssb :: SSB | fx) Json
-- foreign import _testFeed :: ∀ fx. ClientConnection -> String -> EffFnAff (ssb :: SSB | fx) ClientConnection
