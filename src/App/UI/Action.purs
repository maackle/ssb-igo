module App.UI.Action where

import Prelude

import App.IgoMsg (IgoMsg(..), OfferMatchPayload)
import App.IgoMsg as Msg
import App.Streaming (decodeFlumeDb, mapFn, maybeToFlumeState, reduceFn)
import App.UI.Effect (Effect(..), Affect, runEffect)
import App.UI.Model (DevIdentity, FlumeState(..), Model, ScratchOffer)
import App.UI.Optics (ModelLens)
import App.UI.Optics as O
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
import Data.Either (Either(..), either)
import Data.Lens (Lens', set, (.~))
import Data.Lens as Lens
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (sequence)
import Debug.Trace (spy, traceA, traceAny, traceAnyA)
import Halogen.VDom.DOM.Prop (ElemRef(..))
import Spork.App (lift, purely)
import Spork.App as App
import Spork.Html (ElementRef)
import Ssb.Types (UserKey)
import Text.Parsing.Parser.Token (letter)

data Action
  = Noop
  | UpdateFlume Json
  | UpdateFriends Json
  | UpdateIdentity {id :: UserKey}
  -- | UpdateScratch Event (String -> ScratchOffer)
  | PlaceStone
  | CreateOffer UserKey OfferMatchPayload
  | SetDevIdentity (DevIdentity)

  | ManageRef String ElementRef
  | UpdateModel (Model -> Model)
  | UpdateField' (String -> Either String (Model -> Model)) String

  | HandlePlayerAutocomplete (ModelLens String) String

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

  ManageRef key ref -> case ref of
    Created el -> purely $ model { refs = M.insert key el model.refs}
    Removed el -> purely $ model { refs = M.delete key model.refs}

  UpdateModel f ->
    { model: f model
    , effects: lift $ (traceAnyA (f model) *> pure Noop)
    }

  UpdateField' f val ->
    let
      effect = liftEff do
        case (f val) of
          -- Left err -> preventDefault event *> traceA err *> pure Noop
          Left err -> traceA err *> (pure $ UpdateModel (set (O.scratchOffer <<< O.errorMsg) $ Just err))
          Right f -> traceA "OK" *> pure $ UpdateModel f
    in { model, effects: lift effect}

  HandlePlayerAutocomplete lens val ->
    purely model
