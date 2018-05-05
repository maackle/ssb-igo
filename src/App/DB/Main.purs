module App.DB.Main where

import Prelude

import App.Streaming (FlumeDb, MapFn, ReduceFnImpl, initialDb, mapFn, reduceFn)
import Control.Monad.Aff.Compat (EffFnAff(..), fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, info)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Function.Uncurried (mkFn2)
import Ssb.Config (SSB)
import Ssb.PullStream (PullStream)
import Ssb.Server (Plugin, toPlugin)

ssbIgoPlugin =
  { init: \sbot -> unsafePerformEff $ init sbot
  , name: "ssbIgo"
  , version: "0.1"
  , manifest
  }
  where
    init sbot = do
      view <- flumeUse sbot "ssb-igo" flumeReducer
      -- info "Plugin initialized!"
      pure
        { streamDb: liveStream view
        , rawGet: _rawGet view  -- NOTE: the FFI here is hosed
        }

    manifest =
      { "streamDb": "source"
      , "rawGet": "async"
      }

flumeReducer :: FlumeReducer
flumeReducer = mkFlumeReducer "0.1" (mkFn2 reduceFn) mapFn initialDb


foreign import data FlumeReducer :: Type
foreign import data FlumeView :: Type
foreign import data Sbot :: Type
foreign import mkFlumeReducer :: String -> ReduceFnImpl -> MapFn -> FlumeDb -> FlumeReducer
foreign import flumeUse :: ∀ fx. Sbot -> String -> FlumeReducer -> Eff (ssb :: SSB | fx) FlumeView
foreign import liveStream :: ∀ fx. FlumeView -> Eff (ssb :: SSB | fx) PullStream
foreign import _rawGet :: ∀ fx. FlumeView -> (FlumeDb -> Unit)   -- NOTE: the FFI here is hosed
