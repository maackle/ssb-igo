module App.UI.View where

import Prelude

import App.IgoMsg (IgoMsg(..))
import App.UI.Action (Action(..))
import App.UI.Model (Model)
import Spork.Html as H
import Spork.Html.Elements.Keyed as K

render :: Model -> H.Html Action
render model =
  H.div []
    [ H.button [H.onClick $ H.always_ (PlaceStone)] [ H.text "publish public"]
    , H.button [H.onClick $ H.always_ (CreateOffer )] [ H.text "publish private"] ]
