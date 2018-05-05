module Ssb.Common where


import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..))
import Control.Monad.Eff (Eff)
import Ssb.Config (SSB)


type SA f a = Aff (ssb :: SSB | f) a
type SA' f a = EffFnAff (ssb :: SSB | f) a

type SE f a = Eff (ssb :: SSB | f) a
