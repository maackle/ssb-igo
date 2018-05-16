module App.UI.Action where

import Prelude

import App.IgoMsg (IgoMsg(OfferMatch), OfferMatchPayload)
import App.IgoMsg as Msg
import App.Streaming (decodeFlumeDb, mapFn, maybeToFlumeState, reduceFn)
import App.UI.Effect (Effect(..))
import App.UI.Model (FlumeState(..), Model)
import Data.Argonaut (Json, jsonNull)
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (spy, traceAny)
import Spork.App (lift, purely)
import Spork.App as App
import Ssb.Types (UserKey)

data Action
  = Noop
  | UpdateFlume Json
  | UpdateIdentity {id :: UserKey}
  | PlaceStone
  | CreateOffer UserKey OfferMatchPayload
  | SetFeedPath (Maybe String)

update ∷ Model -> Action -> App.Transition Effect Model Action
update model = case _ of
  Noop ->
    App.purely model
  UpdateIdentity {id} ->
    App.purely $ model { whoami = Just id }
  UpdateFlume json -> traceAny json \_ ->
    App.purely $ case model.flume of
      FlumeFailure _ -> model
      FlumeUnloaded -> model { flume = maybeToFlumeState "failed loading state" $ decodeFlumeDb json }
      FlumeDb flume ->
        let mapped = mapFn json
        in if spy $ mapped == jsonNull
          then model
          else model { flume = FlumeDb $ reduceFn flume mapped }
  PlaceStone ->
    { model, effects: lift (Publish model.feedPath (Msg.demoMsg) Noop) }
  CreateOffer opponent payload ->
    let msg = OfferMatch payload
    in { model, effects: lift (Publish model.feedPath msg Noop)}
  SetFeedPath path ->
    { model: model { feedPath = path }
    , effects: lift (GetIdentity model.feedPath UpdateIdentity)
    }
