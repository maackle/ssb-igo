module App.UI.ClientQueries where

import Prelude

import App.Streaming (FlumeDb)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Ssb.Client (ClientConnection)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)

getDb = fromEffFnAff <<< _getDb

foreign import getStream :: ∀ fx. ClientConnection -> Eff (ssb :: SSB | fx) PullStream
foreign import _getDb :: ∀ fx. ClientConnection -> EffFnAff (ssb :: SSB | fx) FlumeDb
