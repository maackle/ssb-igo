module App.UI.View.MakeOffer where

import App.UI.Action (Action)
import App.UI.Model (EzModel)
import Spork.Html as H

offerForm :: EzModel -> H.Html Action
offerForm ez =
  H.section []
    [ H.form [] []
    ]
