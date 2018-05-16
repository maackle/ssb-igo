module App.UI.View where

import Prelude

import App.IgoMsg (demoOfferPayload)
import App.Streaming (encodeFlumeDb)
import App.UI.Action (Action(..))
import App.UI.Model (FlumeState(..), IndexedRequest(..), Model, EzModel, ezify)
import App.UI.View.Dashboard as View
import App.UI.View.Dev as Dev
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.StrMap as M
import Global.Unsafe (unsafeStringify)
import Spork.Html as H

testIdentity = "PhgZSAy4aWPYx231rgypWz8jjNOJmwCi9diVYiYHh50=.ed25519"
otherIdentity = "70mCOxEUBDup8sP1ec7XjCQqJmN6/XQDVf7wRKyjEvQ=.ed25519"
demoOffer = demoOfferPayload otherIdentity

render :: Model -> H.Html Action
render model =
  case ezify model of
    Nothing -> H.div []
      [ H.h1 [] [H.text "app loading..."]
      , H.pre []
        [ H.text (unsafeStringify model)]
      ]
    Just ez ->
      H.div []
        [ Dev.devToolbar
        , View.dashboard ez
        , H.button [H.onClick $ H.always_ (PlaceStone)] [ H.text "publish public"]
        , H.button
          [ H.onClick $ H.always_ (CreateOffer testIdentity demoOffer ) ]
          [ H.text "publish private" ]
        , H.pre []
          [ H.text (unsafeStringify model)]
        ]
  where
    showDb = case model.flume of
      FlumeDb d -> show $ encodeFlumeDb d
      d -> unsafeStringify d

dashboard :: EzModel -> H.Html Action
dashboard ez@{db} =
  H.div []
    [ H.section []
      [ H.h1 [] [H.text "requests"]
      , myRequest ez
      ]
    ]

myRequest {db, whoami} =
  case r of
    Just request@(IndexedRequest {terms} {key}) ->
      requestLine request
    Nothing ->
      H.text "make a request?"
  where
    r = (M.values db.requests) # find \(IndexedRequest d {author}) ->
      author == whoami


requestLine (IndexedRequest {terms} _) =
  H.li [] [H.text $ show terms.size]
