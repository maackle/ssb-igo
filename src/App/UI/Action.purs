module App.UI.Action where

import Prelude

import App.IgoMsg as Msg
import App.UI.Effect (Effect(..))
import App.UI.Model (Model)
import Data.Argonaut (Json)
import Spork.App as App

data Action
  = Noop
  | PlaceStone
  | CreateOffer
  | ReduceIgoMessage Json


testIdentity = "PhgZSAy4aWPYx231rgypWz8jjNOJmwCi9diVYiYHh50=.ed25519"
otherIdentity = "70mCOxEUBDup8sP1ec7XjCQqJmN6/XQDVf7wRKyjEvQ=.ed25519"

update ∷ Model -> Action -> App.Transition Effect Model Action
update model = case _ of
  Noop ->
    App.purely model
  PlaceStone ->
    let
      effects = App.lift (Publish (Msg.demoMsg) Noop)
    in
      { model, effects }
  CreateOffer ->
    { model, effects: App.lift (PublishPrivate Msg.demoPrivate [testIdentity] Noop)}
  ReduceIgoMessage json ->
    { model, effects: App.lift (Log ("hahaha  " <> show json) Noop) }
