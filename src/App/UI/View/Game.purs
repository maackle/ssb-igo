module App.UI.View.Game where

import Prelude

import App.Common (div_)
import App.IgoMsg (IgoMsg(..), StoneColor(..))
import App.UI.Action (Action(..))
import App.UI.Model (EzModel, IndexedMatch(..), IndexedOffer(IndexedOffer), IndexedRequest(IndexedRequest), Model, assignColors')
import App.UI.Routes (Route(..))
import App.UI.View.Components (link, userKeyMarkup)
import App.UI.View.MakeOffer (offerForm)
import Data.Array (filter, intercalate, length, singleton)
import Data.Array.NonEmpty (replicate)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Spork.Html (classes)
import Spork.Html as H


viewGame :: Model -> EzModel -> Maybe IndexedMatch -> H.Html Action
viewGame model ez@{db, whoami} maybeMatch = case maybeMatch of
  Just match@(IndexedMatch {offerPayload}) ->
    let
      gameDiv = H.div
        [ H.classes ["tenuki-board"]
        , H.ref (Just <<< (ManageTenukiGame match)) ]
        []
      datum k v =
        H.div [H.classes $ ["datum", k]]
          [ div_ "key" [H.text k]
          , div_ "val" [H.text v]
          ]
      blackPlayer = H.div [H.classes ["player", "my-turn"]]
        [ div_ "turn-notification" [H.text "your turn"]
        , div_ "name" [H.text "maackle"]
        , div_ "caps" [H.text "captures: 5"]
        ]
      whitePlayer = blackPlayer

      passButton = H.button [H.classes ["pass"]] [H.text "pass"]
      resignButton = H.button [H.classes ["resign"]] [H.text "resign"]

      kibitzes =
        [ div_ "kibitz"
          [ div_ "author" [H.text "mijkl"]
          , div_ "message" [H.text "hey what's up"]
          ]
        ]

    in div_ "game-content"
      [ gameDiv
      , div_ "panel"
        [ div_ "players"
          [ blackPlayer, whitePlayer]
        , div_ "game-info"
          [ div_ "controls"
            [ passButton, resignButton]
          , datum "move" "101"
          , datum "komi" "0.5"
          , datum "handicap" "2"
          ]
        , div_ "kibitz-container"
          [ div_ "kibitzes" kibitzes
          , div_ "kibitz-input"
            [ H.input [H.type_ H.InputText, H.placeholder "Type to talk..."]
            , H.button [] [H.text "Chat"]
            ]
          ]
      ]
      , link Dashboard H.a [H.classes ["close"]] [H.text "×"]
      ]
  Nothing -> H.text "NO GAME FOUND"
