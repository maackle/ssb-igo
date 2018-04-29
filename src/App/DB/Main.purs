module App.DB.Main where

import Prelude

import App.IgoMsg (IgoMsg(..))
import Control.Monad.Eff (Eff)
import Data.Foreign (Foreign)
import Ssb.Config (SSB)


plugin =
  { init: init
  , name: "ssbIgoDb"
  , version: "0.1"
  , manifest: {}
  }

init sbot = do
  view <- flumeUse sbot "ssb-igo-index" flumeReducer
  stream <- liveStream view
  pure "TODO"
  -- pure { streamDb: streamDb }



type FlumeDb = {}

type ReduceFn = FlumeDb -> IgoMsg -> FlumeDb
type MapFn = Foreign -> IgoMsg

reduceFn :: ReduceFn
reduceFn db = case _ of
  RequestMatch terms -> {}
  _ -> {}

mapFn :: MapFn
mapFn _ = Kibitz { move: "D7", text: "TODO" }

flumeReducer = mkFlumeReducer "0.1" reduceFn mapFn {}


foreign import data FlumeReducer :: Type
foreign import data FlumeView :: Type
foreign import data PullStream :: Type
foreign import data Sbot :: Type
foreign import mkFlumeReducer :: String -> ReduceFn -> MapFn -> FlumeDb -> FlumeReducer
foreign import flumeUse :: ∀ fx. Sbot -> String -> FlumeReducer -> Eff (ssb :: SSB | fx) FlumeView
foreign import liveStream :: ∀ fx. FlumeView -> Eff (ssb :: SSB | fx) PullStream
