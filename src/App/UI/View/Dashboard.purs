module App.UI.View.Dashboard where

import Prelude

import App.Common (div_)
import App.Flume (IndexedMatch(..), IndexedOffer(IndexedOffer), IndexedRequest(IndexedRequest), assignColors', isMatchFinalized, moveNumber)
import App.IgoMsg (IgoMsg(..), StoneColor(..))
import App.UI.Action (Action(..))
import App.UI.Model (EzModel, Model)
import App.UI.Optics as O
import App.UI.Routes (Route(..))
import App.UI.View.Components (link, userKeyMarkup)
import App.UI.View.MakeOffer (offerForm)
import Data.Array (filter, intercalate, length, singleton)
import Data.Array.NonEmpty (replicate)
import Data.Foldable (find)
import Data.Lens as Lens
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Spork.Html (classes)
import Spork.Html as H

class' = classes <<< singleton

dashboard :: Model -> EzModel -> H.Html Action
dashboard model ez@{db, whoami} =
  div_ "container dashboard"
    [ H.section []
      [ H.h2 [] [H.text "requests"]
      , H.lazy myRequest ez

      , H.h2 [] [H.text "offers"]
      , H.button
        [ class' "btn-main", H.onClick $ H.always_ $ UpdateModel (Lens.over O.showOfferForm not) ]
        [ H.text "Make offer"]
      , if model.showOfferForm
        then H.lazy2 offerForm model ez
        else H.div [] []
      , H.table [class' ""]
        [ H.thead [] offerHeaders
        , H.tbody [] (map (offerRow model) incomingOffers)
        ]

      , H.h2 [] [H.text "my games"]
      , H.table [class' "your-games"]
        [ H.thead [] gameHeaders
        , H.tbody [] (map (gameRow model) myGames)
        ]

      , H.h2 [] [H.text "active games"]
      , H.table []
        [ H.thead [] gameHeaders
        , H.tbody [] (map (gameRow model) otherGames)
        ]

      , H.h2 [] [H.text "completed games"]
      , H.table []
        [ H.thead [] gameHeaders
        , H.tbody [] (map (gameRow model) finalizedGames)
        ]
      ]
    ]
  where
    incomingOffers :: Array IndexedOffer
    incomingOffers = M.values db.offers
      # filter \(IndexedOffer {opponentKey} _) -> opponentKey == whoami

    finalizedGames :: Array IndexedMatch
    finalizedGames = M.values db.matches # filter isMatchFinalized

    normalGames = M.values db.matches # filter (not <<< isMatchFinalized)

    myGames :: Array IndexedMatch
    myGames = normalGames
      # filter \(IndexedMatch {acceptMeta, offerMeta}) ->
        acceptMeta.author == whoami || offerMeta.author == whoami

    otherGames :: Array IndexedMatch
    otherGames = normalGames
      # filter \(IndexedMatch {acceptMeta, offerMeta}) ->
        not $ (acceptMeta.author == whoami || offerMeta.author == whoami)



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
      [ H.button
        [H.onClick handleAccept]
        [H.text "Accept"]
      , H.button
        [H.onClick handleDecline]
        [H.text "Decline"]
      ]
    ]
  where
    from = userKeyMarkup model author
    stoneIcon = case myColor of
      White -> "●"
      Black -> "○"
    -- size e.g. 19x19
    size = intercalate "x" $ replicate 2 $ show terms.size
    handleAccept = H.always_ $ Publish $ AcceptMatch
      { offerKey: key, terms: terms }
    handleDecline = H.always_ $ Publish $ DeclineMatch
      { offerKey: key, reason: Nothing }


gameHeaders =
  [ H.th [] [H.text "Black"]
  , H.th [] [H.text "White"]
  , H.th [] [H.text "Move"]
  , H.th [] [H.text ""]
  , H.th [] [H.text ""]
  ]


gameRow :: Model -> IndexedMatch -> (H.Html Action)
gameRow model match@(IndexedMatch {offerPayload, acceptPayload, moves, offerMeta, acceptMeta}) =
  H.tr []
    [ H.td [] [userKeyMarkup model black]
    , H.td [] [userKeyMarkup model white]
    , H.td [] [H.text $ show $ moveNumber match]
    , H.td []
      [ link (ViewGame acceptMeta.key) H.button [class' "btn-main"] [H.text "view"]]
    ]
  where
    {black, white} = assignColors' offerPayload offerMeta
