module App.UI.View.Dev where

import Prelude

import App.UI.Action (Action(..))
import App.UI.ClientQueries (testFeeds)
import Data.Maybe (Maybe(..))
import Spork.Html as H

devToolbar =
  H.div [H.classes ["dev-toolbar"]]
    [ H.a [H.href "#", H.onClick $ const $ Just $ SetFeedPath (Just testFeeds.alice)]
      [ H.text "set alice" ]
    ]
