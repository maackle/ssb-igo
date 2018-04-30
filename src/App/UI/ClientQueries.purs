module App.UI.ClientQueries
  ( getStream ) where


import Control.Monad.Eff (Eff)
import Ssb.Client (ClientConnection)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)

foreign import getStream :: ∀ fx. ClientConnection -> Eff (ssb :: SSB | fx) PullStream
