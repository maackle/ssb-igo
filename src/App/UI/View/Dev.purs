module App.UI.View.Dev where

import Prelude

import App.UI.Action (Action(..))
import App.UI.Model (DevIdentity(..), EzModel)
import Data.Maybe (Maybe(..))
import Spork.Html as H

devToolbar :: EzModel -> H.Html Action
devToolbar ez =
  H.div [H.classes ["dev-toolbar"], H.style "display: flex; width: 100%"]
    [ H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Alice]
      [ H.text "alice" ]
    , H.text " || "
    , H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Bob]
      [ H.text "bob" ]
    , H.text " || "
    , H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Charlie]
      [ H.text "charlie" ]
    , H.text " "
    , H.span [] [H.text ez.whoami]
    ]
