module App.UI.View.Game where

import Prelude

import App.IgoMsg (IgoMsg(..), StoneColor(..))
import App.UI.Action (Action(..))
import App.UI.Model (EzModel, IndexedMatch(..), IndexedOffer(IndexedOffer), IndexedRequest(IndexedRequest), Model, assignColors')
import App.UI.View.Components (userKeyMarkup)
import App.UI.View.MakeOffer (offerForm)
import Data.Array (filter, intercalate, length, singleton)
import Data.Array.NonEmpty (replicate)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Spork.Html (classes)
import Spork.Html as H

class' = classes <<< singleton

viewGame :: Model -> EzModel -> Maybe IndexedMatch -> H.Html Action
viewGame model ez@{db, whoami} maybeMatch = case maybeMatch of
  Just match ->
    H.div []
      [ H.text "A GAME"
      ]
  Nothing -> H.text "NO GAME FOUND"
