module App.UI.View where

import Prelude

import App.IgoMsg (IgoMsg(..), demoOfferPayload)
import App.Streaming (encodeFlumeDb)
import App.UI.Action (Action(..))
import App.UI.Model (FlumeState(..), Model)
import Global.Unsafe (unsafeStringify)
import Spork.Html as H
import Spork.Html.Elements.Keyed as K

testIdentity = "PhgZSAy4aWPYx231rgypWz8jjNOJmwCi9diVYiYHh50=.ed25519"
otherIdentity = "70mCOxEUBDup8sP1ec7XjCQqJmN6/XQDVf7wRKyjEvQ=.ed25519"
demoOffer = demoOfferPayload otherIdentity

render :: Model -> H.Html Action
render model =
  H.div []
    [ H.button [H.onClick $ H.always_ (PlaceStone)] [ H.text "publish public"]
    , H.button
      [ H.onClick $ H.always_ (CreateOffer testIdentity demoOffer ) ]
      [ H.text "publish private!" ]
    , H.pre []
      [ H.text (showDb)]
    ]
  where
    showDb = case model.flume of
      FlumeDb d -> show $ encodeFlumeDb d
      d -> unsafeStringify d
