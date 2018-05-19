module Ssb.PullStream where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)

drainWith stream fn = fromEffFnAff $ _drainWith stream fn

foreign import data PullStream :: Type
foreign import _drainWith :: ∀ fx. PullStream -> (Json -> Eff fx Boolean) -> EffFnAff fx Unit
