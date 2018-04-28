module Ssb.Client
  ( getClient
  , publish
  , ClientF
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)
import Ssb.Config (Config, SSB)

foreign import data ClientF :: Type

type SF f a = Aff (ssb :: SSB | f) a

foreign import _getClient :: ∀ f. Config -> EffFnAff (ssb :: SSB | f) ClientF
foreign import _publish :: ∀ f. Fn2 ClientF Json (EffFnAff (ssb :: SSB | f) Json)

getClient :: ∀ f. Config -> SF f ClientF
getClient config = fromEffFnAff $ _getClient config

publish :: ∀ f. ClientF -> Json -> SF f Json
publish client msg = fromEffFnAff (runFn2 _publish client msg)

-- getClient :: SsbEff ClientInstance
-- getClient


-- demo = do
--   client <- getClient
--   publish client
