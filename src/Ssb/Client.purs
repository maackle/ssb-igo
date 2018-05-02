module Ssb.Client where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (Json)
import Ssb.Config (Config, SSB)
import Ssb.Types (UserKey)

foreign import data ClientConnection :: Type

type SA f a = Aff (ssb :: SSB | f) a
type SA' f a = EffFnAff (ssb :: SSB | f) a

type SE f a = Eff (ssb :: SSB | f) a

-- Eff

foreign import unboxPrivate :: ∀ fx. ClientConnection -> Json -> (SE fx Json)

-- Aff

close :: ∀ fx. ClientConnection -> SA fx Unit
close = fromEffFnAff <<< _close

getClient :: ∀ fx. Config -> SA fx ClientConnection
getClient = fromEffFnAff <<< _getClient

publish :: ∀ fx. ClientConnection -> Json -> SA fx Json
publish client msg = fromEffFnAff $ _publish client msg

publishPrivate :: ∀ fx. ClientConnection -> Json -> Array UserKey -> SA fx Json
publishPrivate client msg recips = fromEffFnAff $ _publishPrivate client msg recips

whoami :: ∀ fx. ClientConnection -> SA fx {id :: UserKey}
whoami = fromEffFnAff <<< _whoami

foreign import _close :: ∀ fx. ClientConnection -> SA' fx Unit
foreign import _getClient :: ∀ fx. Config -> SA' fx ClientConnection
foreign import _publish :: ∀ fx. ClientConnection -> Json -> (SA' fx Json)
foreign import _publishPrivate :: ∀ fx. ClientConnection -> Json -> Array UserKey -> (SA' fx Json)
foreign import _whoami :: ∀ fx. ClientConnection -> (SA' fx {id :: UserKey})
