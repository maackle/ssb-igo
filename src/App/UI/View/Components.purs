module App.UI.View.Components where

import Prelude

import App.UI.Action (Action)
import App.UI.Model (Model)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Data.String as String
import Spork.Html as H
import Ssb.Types (UserKey)

truncatedKey :: String -> String
truncatedKey key = String.take 8 key <> "..."

userKeyMarkup :: Model -> UserKey -> H.Html _
userKeyMarkup {userKeys} userKey = case M.lookup userKey userKeys of
  Just {name, key} ->
    H.a [H.classes ["user-link"], H.title key, H.href "#"]
      [H.text $ maybe (truncatedKey key) ((<>) "@") name]
  Nothing ->
    H.text $ truncatedKey userKey
