module App.UI.View.Dev where

import Prelude

import App.UI.Action (Action(..))
import App.UI.ClientQueries (devClient)
import App.UI.Model (DevIdentity(..))
import Data.Maybe (Maybe(..))
import Spork.Html as H

devToolbar ez =
  H.div [H.classes ["dev-toolbar"], H.style "display: flex; width: 100%"]
    [ H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Alice]
      [ H.text "alice" ]
    , H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Bob]
      [ H.text "bob" ]
    , H.a [H.href "#", H.onClick $ const $ Just $ SetDevIdentity Charlie]
      [ H.text "charlie" ]
    , H.span [] [H.text ez.whoami]
    ]
