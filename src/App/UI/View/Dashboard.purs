module App.UI.View.Dashboard where

import Prelude

import App.IgoMsg (StoneColor(..))
import App.UI.Action (Action)
import App.UI.Model (EzModel, IndexedOffer(IndexedOffer), IndexedRequest(IndexedRequest), Model, userNameFromKey)
import App.UI.View.Components (userKeyMarkup)
import App.UI.View.MakeOffer (offerForm)
import Data.Array (filter, intercalate, singleton)
import Data.Array.NonEmpty (replicate)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Spork.Html (classes)
import Spork.Html as H

class' = classes <<< singleton

dashboard :: Model -> EzModel -> H.Html Action
dashboard model ez@{db, whoami} =
  H.div []
    [ H.section []
      [ H.h1 [] [H.text "requests"]
      , myRequest ez
      , H.h2 [] [H.text "offers"]
      , H.table []
        [ H.thead [] offerHeaders
        , H.tbody [] (map (offerRow model) incomingOffers)
        ]
      ]
    , H.lazy2 offerForm model ez
    ]
  where
    incomingOffers :: Array IndexedOffer
    incomingOffers = M.values db.offers
      # filter \(IndexedOffer {opponentKey} _) -> opponentKey == whoami
--
-- table :: Array String -> Array String -> Array (H.Html Action) -> H.Html Action
-- table classNames headers rows =
--   H.table []
--     [ H.thead []
--       headElement
--   where
--     headElements = zip headers classNames # map \(title & className) ->
--       H.th [classes [className]] [H.text title]
--

myRequest :: EzModel -> H.Html Action
myRequest {db, whoami} =
  case r of
    Just request@(IndexedRequest {terms} {key}) ->
      requestLine request
    Nothing ->
      H.text "make a request?"
  where
    r = (M.values db.requests) # find \(IndexedRequest _ {author}) ->
      author == whoami

requestLine :: IndexedRequest -> H.Html Action
requestLine (IndexedRequest {terms} _) =
  H.li [] [H.text $ show terms.size]

-- myOffers :: EzModel -> H.Html Action
-- myOffers {db, whoami} =
--     H.div [] $ map offerLine offers
--   where
--     offers = (M.values db.offers) # filter \(IndexedOffer {opponentKey} _) ->
--       opponentKey == whoami

-- offerLine :: IndexedOffer -> H.Html Action
-- offerLine (IndexedOffer {terms, myColor} {author}) =
--   H.li [] [H.text $ show terms.size]

offerHeaders =
  [ H.th [] [H.text "From"]
  , H.th [] [H.text "Size"]
  , H.th [] [H.text "Handicap"]
  , H.th [] [H.text "Komi"]
  , H.th [] [H.text ""]
  ]

offerRow :: Model -> IndexedOffer -> H.Html Action
offerRow model (IndexedOffer {terms, myColor} {author, key}) =
  H.tr []
    [ H.td [] [from, H.text " ", H.span [] [H.text stoneIcon]]
    , H.td [] [H.text size]
    , H.td [] [H.text $ show terms.handicap]
    , H.td [] [H.text $ show terms.komi]
    , H.td []
      [ H.button [] [H.text "Accept"]
      , H.button [] [H.text "Decline"]
      ]
    ]
  where
    from = userKeyMarkup model author
    stoneIcon = case myColor of
      White -> "●"
      Black -> "○"
    -- size e.g. 19x19
    size = intercalate "x" $ replicate 2 $ show terms.size
