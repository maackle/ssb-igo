module Ssb.Client where

import Prelude

import Control.Monad.Aff.Compat (fromEffFnAff)
import Data.Argonaut (Json)
import Data.Foreign (Foreign, toForeign)
import Ssb.Common (SA, SA', SE)
import Ssb.Config (Config(TestConfig, Config))
import Ssb.Types (UserKey)

foreign import data ClientConnection :: Type

foreign import props :: ClientConnection -> {id :: String}

-- Eff

foreign import unboxPrivate :: ∀ fx. ClientConnection -> Json -> (SE fx Json)

-- Aff

close :: ∀ fx. ClientConnection -> SA fx Unit
close = fromEffFnAff <<< _close

getClient :: ∀ fx. Config -> SA fx ClientConnection
getClient = case _ of
  Config cfg -> fromEffFnAff $ _getClient $ toForeign cfg
  TestConfig cfg -> fromEffFnAff $ _getClient $ toForeign cfg

publish :: ∀ fx. ClientConnection -> Json -> SA fx Json
publish client msg = fromEffFnAff $ _publish client msg

publishPrivate :: ∀ fx. ClientConnection -> Json -> Array UserKey -> SA fx Json
publishPrivate client msg recips = fromEffFnAff $ _publishPrivate client msg recips

whoami :: ∀ fx. ClientConnection -> SA fx {id :: UserKey}
whoami = fromEffFnAff <<< _whoami

foreign import _close :: ∀ fx. ClientConnection -> SA' fx Unit
foreign import _getClient :: ∀ fx. Foreign -> SA' fx ClientConnection
foreign import _publish :: ∀ fx. ClientConnection -> Json -> (SA' fx Json)
foreign import _publishPrivate :: ∀ fx. ClientConnection -> Json -> Array UserKey -> (SA' fx Json)
foreign import _whoami :: ∀ fx. ClientConnection -> (SA' fx {id :: UserKey})
