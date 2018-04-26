module Ssb.Client
  ( getClient
  , publish
  , SSB
  , ClientF
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn2, runFn2)

foreign import data SSB :: Effect
foreign import data ClientF :: Type

type SF f a = Aff (ssb :: SSB | f) a

foreign import _getClient :: ∀ f. EffFnAff (ssb :: SSB | f) ClientF
foreign import _publish :: ∀ f. Fn2 ClientF Json (EffFnAff (ssb :: SSB | f) Json)

getClient :: ∀ f. SF f ClientF
getClient = fromEffFnAff _getClient

publish :: ∀ f. ClientF -> Json -> SF f Json
publish client msg = fromEffFnAff (runFn2 _publish client msg)

-- getClient :: SsbEff ClientInstance
-- getClient


-- demo = do
--   client <- getClient
--   publish client
