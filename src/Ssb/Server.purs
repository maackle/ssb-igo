module Ssb.Server where

import Prelude

import App.Utils (trace')
import Control.Monad.Aff.Compat (fromEffFnAff)
import Control.Monad.Eff (Eff)
import Data.Argonaut (Json)
import Data.Foreign (Foreign, toForeign)
import Debug.Trace (traceAny)
import Ssb.Common (SE, SA')
import Ssb.Config (Config(..), SSB, addTemp)
import Ssb.Types (UserKey, SsbKeys)

foreign import data Plugin :: Type
foreign import data Sbot :: Type

configBuilder :: Config -> Foreign
configBuilder = case _ of
  Config c -> toForeign c
  TestConfig c -> toForeign $ addTemp c

sbotBuilder :: ∀ fx. Array Plugin -> SE fx (Config -> SE fx Sbot)
sbotBuilder plugins = do
  builder <- _sbotBuilder plugins
  pure $ (builder <<< configBuilder)

loadPlugins :: ∀ fx. Array Plugin -> SE fx Unit
loadPlugins plugins = do
  builder <- _sbotBuilder plugins
  pure unit

createFeed = _createFeed
createFeed' = _createFeed1
createFeedRemote' sbot keys = _createFeed2 sbot keys $ toForeign {remote: true}

foreign import _createFeed :: ∀ fx. Sbot -> SE fx Sbot
foreign import _createFeed1 :: ∀ fx. Sbot -> SsbKeys -> SE fx Sbot
foreign import _createFeed2 :: ∀ fx. Sbot -> SsbKeys -> Foreign -> SE fx Sbot
foreign import requirePlugin :: ∀ fx. String -> SE fx Plugin
foreign import _sbotBuilder :: ∀ fx. Array Plugin -> SE fx (∀ fx'. Foreign -> SE fx' Sbot)
foreign import toPlugin :: ∀ a. a -> Plugin
-- foreign import _startSbot :: ∀ fx'. Foreign -> SE fx' Sbot

-- whoami = fromEffFnAff <<< _whoami
-- foreign import _whoami :: ∀ fx. Sbot -> (SA' fx {id :: UserKey})
