module Ssb.Keys where


import Control.Monad.Eff (Eff)
import Ssb.Config (SSB, Keys)


loadOrCreateSync = _loadOrCreateSync

foreign import _loadOrCreateSync :: ∀ fx. Eff (ssb :: SSB | fx) Keys
