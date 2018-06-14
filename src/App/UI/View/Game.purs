module App.UI.View.Game where

import Prelude

import App.Common (div_)
import App.Flume (IndexedMatch(..), IndexedOffer(IndexedOffer), IndexedRequest(IndexedRequest), KibitzStep(..), assignColors, assignColors', lastMoveKey, nextMover)
import App.IgoMsg (IgoMsg(..), StoneColor(..))
import App.UI.Action (Action(..))
import App.UI.Model (EzModel, Model, userNameFromKey)
import App.UI.Optics as O
import App.UI.Routes (Route(..))
import App.UI.View.Components (link, userKeyMarkup)
import App.UI.View.MakeOffer (offerForm)
import Data.Array (filter, intercalate, length, singleton)
import Data.Array.NonEmpty (replicate)
import Data.Foldable (find)
import Data.Lens as Lens
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Debug.Trace (spy, trace, traceAny)
import Spork.Html (classes)
import Spork.Html as H
import Tenuki.Game as Tenuki


viewGame :: Model -> EzModel -> Maybe IndexedMatch -> H.Html Action
viewGame model@{tenukiGame} ez@{db, whoami} maybeMatch = case maybeMatch of
  Just match@(IndexedMatch {offerPayload, kibitzes}) ->
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
      gameState = Tenuki.currentState <$> tenukiGame
      blackCaps = maybe 0 _.blackStonesCaptured gameState
      whiteCaps = maybe 0 _.whiteStonesCaptured gameState
      {white, black} = assignColors match
      turnKey = nextMover db match
      playerClasses key = H.classes
        if turnKey /= key
        then ["player"]
        else if key == whoami
        then ["player", "my-turn"]
        else ["player", "their-turn"]
      blackPlayer = H.div [playerClasses black]
        [ div_ "turn-notification" [H.text "your turn"]
        , div_ "name" [H.text $ userNameFromKey model black]
        , div_ "caps" [H.text $ "captures: " <> show whiteCaps]
        ]
      whitePlayer = H.div [playerClasses white]
        [ div_ "turn-notification" [H.text "your turn"]
        , div_ "name" [H.text $ userNameFromKey model white]
        , div_ "caps" [H.text $ "captures: " <> show blackCaps]
        ]

      passButton = H.button [H.classes ["pass"]] [H.text "pass"]
      resignButton = H.button [H.classes ["resign"]] [H.text "resign"]

      kibitzPanel = kibitzes <#> \(KibitzStep {text, author}) ->
        div_ "kibitz"
          [ div_ "author" [H.text $ userNameFromKey model author]
          , div_ "message" [H.text text]
          ]
      move = lastMoveKey match
      text = model.kibitzDraft
      handleKibitzInput = Just <<< UpdateModel <<< (Lens.set O.kibitzDraft)
      handleKibitzSend = H.always_ $ Publish $ Kibitz {text, move}

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
          , datum "hand." "2"
          ]
        , div_ "kibitz-container"
          [ div_ "kibitzes" kibitzPanel
          , div_ "kibitz-input"
            [ H.input
              [ H.type_ H.InputText
              , H.placeholder "Type to talk..."
              , H.onValueInput handleKibitzInput
              ]
            , H.button
              [ H.onClick handleKibitzSend ]
              [ H.text "Chat" ]
            ]
          ]
      ]
      , link Dashboard H.a [H.classes ["close"]] [H.text "×"]
      ]
  Nothing -> H.text "NO GAME FOUND"
