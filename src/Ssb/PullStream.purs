module Ssb.PullStream where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)


foreign import data PullStream :: Type

foreign import drain :: ∀ fx. PullStream -> (Json -> Eff fx Boolean) -> Aff fx Unit
