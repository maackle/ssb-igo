module Ssb.Client where

import Prelude

import Control.Monad.Aff.Compat (fromEffFnAff)
import Data.Argonaut (Json)
import Data.Foreign (Foreign, toForeign)
import Ssb.Common (SA, SA', SE)
import Ssb.Config (Config(TestConfig, Config))
import Ssb.Types (SbotConn, UserKey)

foreign import props :: SbotConn -> {id :: String}

-- Eff

foreign import unboxPrivate :: ∀ fx. SbotConn -> Json -> (SE fx Json)

-- Aff

close :: ∀ fx. SbotConn -> SA fx Unit
close = fromEffFnAff <<< _close

getClient :: ∀ fx. Config -> SA fx SbotConn
getClient = case _ of
  Config cfg -> fromEffFnAff $ _getClient $ toForeign cfg
  TestConfig cfg -> fromEffFnAff $ _getClient $ toForeign cfg

publish :: ∀ fx. SbotConn -> Json -> SA fx Json
publish client msg = fromEffFnAff $ _publish client msg

publishPrivate :: ∀ fx. SbotConn -> Json -> Array UserKey -> SA fx Json
publishPrivate client msg recips = fromEffFnAff $ _publishPrivate client msg recips

whoami :: ∀ fx. SbotConn -> SA fx {id :: UserKey}
whoami = fromEffFnAff <<< _whoami

foreign import _close :: ∀ fx. SbotConn -> SA' fx Unit
foreign import _getClient :: ∀ fx. Foreign -> SA' fx SbotConn
foreign import _publish :: ∀ fx. SbotConn -> Json -> (SA' fx Json)
foreign import _publishPrivate :: ∀ fx. SbotConn -> Json -> Array UserKey -> (SA' fx Json)
foreign import _whoami :: ∀ fx. SbotConn -> (SA' fx {id :: UserKey})
