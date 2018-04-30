module App.DB.Main where

import Prelude

import App.IgoMsg (IgoMsg(..))
import App.Streaming (MapFn, ReduceFn, FlumeDb, mapFn, reduceFn)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (Json, fromObject, fromString)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Foreign (Foreign, toForeign)
import Data.StrMap (StrMap)
import Data.StrMap as M
import Data.Tuple (Tuple(..))
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)

ssbIgoPlugin =
  { init: \sbot -> unsafePerformEff $ init sbot
  , name: "ssbIgoDb"
  , version: "0.1"
  , manifest
  }
  where
    init sbot = do
      view <- flumeUse sbot "ssb-igo-index" flumeReducer
      log "OK!!"
      stream <- liveStream view
      pure { streamDb: const stream }

    manifest = { "streamDb": "source" }

flumeReducer = mkFlumeReducer "0.1" reduceFn mapFn {}

foreign import data FlumeReducer :: Type
foreign import data FlumeView :: Type
foreign import data Sbot :: Type
foreign import mkFlumeReducer :: String -> ReduceFn -> MapFn -> FlumeDb -> FlumeReducer
foreign import flumeUse :: ∀ fx. Sbot -> String -> FlumeReducer -> Eff (ssb :: SSB | fx) FlumeView
foreign import liveStream :: ∀ fx. FlumeView -> Eff (ssb :: SSB | fx) PullStream
