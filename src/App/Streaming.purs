module App.Streaming where

import Prelude

import App.IgoMsg (IgoMsg(..))
import Data.Argonaut (Json)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn2)
import Data.Maybe (Maybe(..))


type FlumeDb = {}

type ReduceFn = FlumeDb -> IgoMsg -> FlumeDb
type ReduceFnImpl = Fn2 FlumeDb IgoMsg FlumeDb
type MapFn = Json -> Maybe IgoMsg

reduceFn :: ReduceFn
reduceFn db = case _ of
  RequestMatch terms -> {}
  _ -> {}

mapFn :: MapFn
mapFn _ = Just $ Kibitz { move: "D11", text: "TODO" }
