module App.UI.View where

import Prelude

import App.IgoMsg (IgoMsg(..), demoOfferPayload)
import App.UI.Action (Action(..))
import App.UI.Model (Model)
import Spork.Html as H
import Spork.Html.Elements.Keyed as K

testIdentity = "PhgZSAy4aWPYx231rgypWz8jjNOJmwCi9diVYiYHh50=.ed25519"
otherIdentity = "70mCOxEUBDup8sP1ec7XjCQqJmN6/XQDVf7wRKyjEvQ=.ed25519"

render :: Model -> H.Html Action
render model =
  H.div []
    [ H.button [H.onClick $ H.always_ (PlaceStone)] [ H.text "publish public"]
    , H.button
      [ H.onClick $ H.always_ (CreateOffer testIdentity demoOfferPayload ) ]
      [ H.text "publish private" ]
    ]
