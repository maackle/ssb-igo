module Ssb.Keys where


import Control.Monad.Eff (Eff)
import Ssb.Config (SSB)
import Ssb.Types (SsbKeys)


loadOrCreateSync = _loadOrCreateSync

foreign import _loadOrCreateSync :: ∀ fx. String -> Eff (ssb :: SSB | fx) SsbKeys
