module App.UI.ClientQueries where

import Prelude

import App.Common (devConfig, getClient')
import App.Flume (decodeFlumeDb, FlumeData)
import App.UI.Model (DevIdentity(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(..), maybe')
import Debug.Trace (traceAnyA)
import Partial.Unsafe (unsafeCrashWith)
import Ssb.Client (getClient)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)
import Ssb.Types (SbotConn)

getDb :: ∀ fx. SbotConn -> Aff (ssb :: SSB | fx) FlumeData
getDb sbot = do
  json <- fromEffFnAff $ _getDb sbot
  case decodeFlumeDb json of
    Just db -> pure db
    Nothing -> pure $ unsafeCrashWith "can't decode raw db"

devClient :: ∀ fx. DevIdentity -> Aff (ssb :: SSB | fx) SbotConn
devClient = case _ of
  Alice -> tc "alice" 8081
  Bob -> tc "bob" 8082
  Charlie -> tc "charlie" 8083
  where
    tc name port =
      getClient =<< (liftEff $ devConfig ("./ssb-dev/" <> name) port)

foreign import getStream :: ∀ fx. SbotConn -> Eff (ssb :: SSB | fx) PullStream
foreign import _getDb :: ∀ fx. SbotConn -> EffFnAff (ssb :: SSB | fx) Json
-- foreign import _testFeed :: ∀ fx. SbotConn -> String -> EffFnAff (ssb :: SSB | fx) SbotConn
