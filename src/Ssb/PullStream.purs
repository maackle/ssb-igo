module Ssb.PullStream where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)
 
drain stream fn = fromEffFnAff $ _drain stream fn

foreign import data PullStream :: Type
foreign import _drain :: ∀ fx. PullStream -> (Json -> Eff fx Boolean) -> EffFnAff fx Unit
