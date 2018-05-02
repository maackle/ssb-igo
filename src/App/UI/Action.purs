module App.UI.Action where

import Prelude

import App.IgoMsg (IgoMsg(..), StoneColor, OfferMatchPayload)
import App.IgoMsg as Msg
import App.UI.Effect (Effect(..))
import App.UI.Model (Model)
import Data.Argonaut (Json)
import Spork.App (lift)
import Spork.App as App
import Ssb.Types (UserKey)

data Action
  = Noop
  | PlaceStone
  | CreateOffer UserKey OfferMatchPayload
  | ReduceIgoMessage Json

update ∷ Model -> Action -> App.Transition Effect Model Action
update model = case _ of
  Noop ->
    App.purely model
  PlaceStone ->
    { model, effects: lift (Publish (Msg.demoMsg) Noop) }
  CreateOffer opponent payload ->
    let msg = OfferMatch payload
    in { model, effects: lift (PublishPrivate msg [opponent] Noop)}
  ReduceIgoMessage json ->
    { model, effects: lift (Log ("hahaha  " <> show json) Noop) }
