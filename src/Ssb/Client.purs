module Ssb.Client
  ( getClient
  , publish
  , close
  , ClientConnection
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Ssb.Config (Config, SSB)

foreign import data ClientConnection :: Type

type SF f a = Aff (ssb :: SSB | f) a

foreign import _getClient :: ∀ f. Config -> EffFnAff (ssb :: SSB | f) ClientConnection
foreign import _publish :: ∀ f. ClientConnection -> Json -> (EffFnAff (ssb :: SSB | f) Json)
foreign import _close :: ∀ f. ClientConnection -> EffFnAff (ssb :: SSB | f) Unit

getClient :: ∀ f. Config -> SF f ClientConnection
getClient config = fromEffFnAff $ _getClient config

publish :: ∀ f. ClientConnection -> Json -> SF f Json
publish client msg = fromEffFnAff $ _publish client msg

close :: ∀ f. ClientConnection -> Aff (ssb :: SSB | f) Unit
close = fromEffFnAff <<< _close
