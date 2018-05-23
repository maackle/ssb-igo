module App.UI.View.MakeOffer where

import Prelude

import App.IgoMsg (StoneColor(..))
import App.UI.Action (Action(..))
import App.UI.Model (Model, User, EzModel)
import App.UI.Optics (ModelLens)
import App.UI.Optics as O
import DOM.Classy.Event (toEvent)
import Data.Lens (over, (%~), (.~))
import Data.Maybe (Maybe(..), maybe)
import Spork.Html (classes)
import Spork.Html as H

div_ k = H.div [classes [k]]

playersForm :: Model -> EzModel -> H.Html Action
playersForm {scratchOffer} {whoami} =
  div_ "players-form"
    [ H.button [classes ["switcher"], H.onClick switchEvent] [H.text "switch"]
    , div_ "players"
      [ div_ "player"
        [ H.label [] [H.text "Black"]
        , black
        ]
      , div_ "player"
        [ H.label [] [H.text "White"]
        , white
        ]
      ]
    ]
  where
    {opponent, myColor} = scratchOffer
    opponentDisplay = maybe opponent.key id scratchOffer.opponent.name

    swapColor :: Model -> Model
    swapColor = O.scratchOffer <<< O.myColor %~ case _ of
      Black -> White
      White -> Black

    updateOpponent :: String -> Model -> Model
    updateOpponent name = O.scratchOffer <<< O.opponent %~ \opp -> opp { name = Just name }

    handler = H.always $ (UpdateField updateOpponent)
    opponentField = H.input [H.value opponentDisplay, H.onKeyUp handler]
    switchEvent = H.always $ UpdateModel swapColor true <<< toEvent
    meField = H.text "mijkl"
    {white, black} = case myColor of
      White -> {white: meField, black: opponentField}
      Black -> {black: meField, white: opponentField}


gameTermsForm :: Model -> H.Html Action
gameTermsForm {} =
  H.div []
    [ sizeForm
    , handicapForm
    , komiForm
    ]
  where
    sizeForm =
      H.div [classes ["control-group"]]
        [ H.label [] [H.text "Size"]
        , H.input []
        , H.text " x "
        , H.text "19"
        ]

    handicapForm =
      H.div [classes ["control-group"]]
        [ H.label [] [H.text "Handicap"]
        , H.input []
        ]

    komiForm =
      H.div [classes ["control-group"]]
        [ H.label [] [H.text "Komi"]
        , H.input []
        ]


offerForm :: Model -> EzModel -> H.Html Action
offerForm model ez =
  H.section []
    [ H.form []
      [ playersForm model ez]
    ]
