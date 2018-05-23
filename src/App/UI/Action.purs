module App.UI.Action where

import Prelude

import App.IgoMsg (IgoMsg(..), OfferMatchPayload)
import App.IgoMsg as Msg
import App.Streaming (decodeFlumeDb, mapFn, maybeToFlumeState, reduceFn)
import App.UI.Effect (Effect(..), Affect, runEffect)
import App.UI.Model (DevIdentity, FlumeState(..), Model, ScratchOffer)
import App.UI.Optics (ModelLens)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Eff
import DOM.Classy.Event (preventDefault, target, toEvent)
import DOM.Classy.Node (fromNode, nodeValue, textContent)
import DOM.Event.KeyboardEvent as KeyboardEvent
import DOM.Event.Types (Event, KeyboardEvent)
import DOM.HTML.HTMLInputElement as HTMLInputElement
import DOM.HTML.Types (HTMLInputElement)
import Data.Argonaut (Json, jsonNull)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Debug.Trace (spy, traceAny, traceAnyA)
import Spork.App (lift, purely)
import Spork.App as App
import Ssb.Types (UserKey)
import Text.Parsing.Parser.Token (letter)

data Action
  = Noop
  | UpdateFlume Json
  | UpdateFriends Json
  | UpdateIdentity {id :: UserKey}
  -- | UpdateScratch Event (String -> ScratchOffer)
  | UpdateModel (Model -> Model) Boolean Event
  | PlaceStone
  | CreateOffer UserKey OfferMatchPayload
  | SetDevIdentity (DevIdentity)

  | UpdateField (String -> Model -> Model) KeyboardEvent

data EventDispatcher
  = CurrentTargetDispatch


update ∷ ∀ eff. Model -> Action -> App.Transition (Affect eff) Model Action
update model = case _ of
  Noop ->
    App.purely model
  UpdateIdentity {id} ->
    App.purely $ model { whoami = Just id }
  UpdateFlume json ->
    App.purely $ case model.flume, decodeFlumeDb json of
      FlumeFailure _, _ -> model
      _, Just db -> model { flume = FlumeDb db }
      FlumeUnloaded, Nothing -> model { flume = FlumeFailure "Flume index not intitialized"}
      FlumeDb flume, Nothing ->
        let mapped = mapFn json
        in if spy $ mapped == jsonNull
          then model
          else model { flume = FlumeDb $ reduceFn flume mapped }
  UpdateFriends json ->
    {model, effects: lift $ runEffect $ Log ("FRIENDS  " <> show json) Noop}
  -- UpdateScratch event f ->
  --   let val = target node
  --   in purely $ model { scratchOffer = f model.scratchOffer }
  PlaceStone ->
    { model, effects: lift $ runEffect (Publish model.devIdentity (RequestMatch Msg.defaultRequest) Noop) }
  CreateOffer opponent payload ->
    let msg = OfferMatch payload
    in { model, effects: lift $ runEffect (Publish model.devIdentity msg Noop)}
  SetDevIdentity ident ->
    { model: model { devIdentity = Just ident }
    , effects: lift $ runEffect (GetIdentity (Just ident) UpdateIdentity)
    }

  UpdateModel f prevent event ->
    { model: f model
    , effects: lift $ liftEff $ (if prevent then preventDefault event else pure unit) *> pure Noop
    }
  UpdateField f event ->
    let
      effect = liftEff do
        let
          node :: Maybe HTMLInputElement
          node = fromNode $ target event
        val :: Maybe String <- sequence $ HTMLInputElement.value <$> node
        traceAnyA val
        pure $ UpdateModel (maybe id f val) false (toEvent event)
    in { model, effects: lift effect}
