module App.Streaming where


import App.IgoMsg (IgoMsg(..))
import Data.Foreign (Foreign)


type FlumeDb = {}

type ReduceFn = FlumeDb -> IgoMsg -> FlumeDb
type MapFn = Foreign -> IgoMsg

reduceFn :: ReduceFn
reduceFn db = case _ of
  RequestMatch terms -> {}
  _ -> {}

mapFn :: MapFn
mapFn _ = Kibitz { move: "D7", text: "TODO" }
